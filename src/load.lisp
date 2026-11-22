(in-package #:policy-protocol)

;;; Loading policies from data. Settings files are untrusted input.
;;;
;;; LOAD-POLICY SOURCE &key FORMAT
;;;   SOURCE  a string (the text itself), a pathname (the file's text) or a
;;;           character stream.
;;;   FORMAT  :sexp, :json or :toml. Default: from a pathname's type ("json",
;;;           "toml"; anything else :sexp), else :sexp. Another keyword
;;;           signals POLICY-FORMAT-UNSUPPORTED.
;;;
;;; :sexp  exactly one (defpolicy NAME (OPTION...) BODY...) form (parse.lisp),
;;;        read as data, never evaluated: *READ-EVAL* NIL (so #. is refused),
;;;        standard readtable, *PACKAGE* bound to a temporary package that is
;;;        deleted afterwards, so no symbol is interned in any user package
;;;        (keywords such as :confined are read as keywords; names become
;;;        strings in the model anyway). A reader error, #., no form or more
;;;        than one, or a first form that is not a DEFPOLICY list:
;;;        POLICY-SYNTAX-ERROR (FORM: the text) with USE-VALUE (a value to
;;;        return instead).
;;;
;;; :json / :toml  text decoded by json-protocol / toml-protocol
;;;        (*JSON-BACKEND* / *TOML-BACKEND*), through DECODE-POLICY-DATA,
;;;        whose methods are in the policy-protocol/settings system; without
;;;        it: POLICY-FORMAT-UNSUPPORTED. Nothing is interned. Text that does
;;;        not decode: POLICY-SYNTAX-ERROR with USE-VALUE as above.
;;;        The data is one object (JSON object / TOML document):
;;;          "name"       string, optional
;;;          "combining"  "forbid-overrides" (default) | "last-match"
;;;          "default"    "permit" | "ask" | "forbid", optional
;;;          "rules"      array of rule objects (TOML: [[rules]] tables):
;;;             "effect"       "permit" | "ask" | "forbid"   (required)
;;;             "id"           string
;;;             "action"       string glob
;;;             "resource"     string glob, or an object with exactly one of
;;;                            "glob", "prefix", "path", "host" → a string
;;;             "when"         condition: an array [REL, ARG...] (strings and
;;;                            integers) is an atom; {"not": C},
;;;                            {"and": [C...]}, {"or": [C...]}
;;;             "obligations"  array of arrays [KIND, ARG...]
;;;             "feedback"     string
;;;        Unknown keys, wrong types, an unknown effect or mode:
;;;        POLICY-SYNTAX-ERROR with the same restarts as PARSE-POLICY (per
;;;        rule: SKIP-RULE, USE-VALUE; per option: USE-VALUE).
;;;
;;; The same policy written as sexp, JSON or TOML gives EQUAL POLICY-FORMs
;;; and the same decisions. POLICY-TO-JSON (settings system) writes the
;;; canonical form as JSON in this shape (keys in the order above, "default"
;;; and empty optional keys omitted, deterministic via json-protocol's alist
;;; encoding), and LOAD-POLICY reads it back to an EQUAL POLICY-FORM.

(defgeneric decode-policy-data (format text)
  (:documentation "The POLICY in TEXT (a string) of data FORMAT (:json,
:toml). Methods live in policy-protocol/settings.")
  (:method (format text)
    (declare (ignore text))
    (error 'policy-format-unsupported :format format)))

;;; Sources that cannot be read: POLICY-SYNTAX-ERROR (FORM: the text) with
;;; USE-VALUE, whose value LOAD-POLICY (or DECODE-POLICY-DATA) returns.

(defmacro %with-source-restart (&body body)
  "Run BODY; a USE-VALUE of %UNREADABLE returns its value from here."
  `(catch '%load-policy ,@body))

(defun %unreadable (text reason &rest args)
  (restart-case (error 'policy-syntax-error :form text :reason (apply #'format nil reason args))
    (use-value (value)
      :report "Return a value instead of the policy."
      :interactive (lambda ()
                     (format *query-io* "Value to return: ")
                     (force-output *query-io*)
                     (list (read *query-io*)))
      (throw '%load-policy value))))

(defun %stream-text (stream)
  (with-output-to-string (out)
    (let ((buffer (make-string 4096)))
      (loop for n = (read-sequence buffer stream)
            while (plusp n)
            do (write-string buffer out :end n)))))

(defun %source-text (source)
  "→ (values TEXT FORMAT-OR-NIL)."
  (etypecase source
    (string (values source nil))
    (pathname
     (values (with-open-file (in source :external-format :utf-8) (%stream-text in))
             (let ((type (pathname-type source)))
               (cond ((and (stringp type) (string-equal type "json")) :json)
                     ((and (stringp type) (string-equal type "toml")) :toml)
                     (t :sexp)))))
    (stream (values (%stream-text source) nil))))

;;; :sexp

(defvar *%sexp-readtable* nil)

(defun %sexp-readtable ()
  "The standard readtable with #S refused (it would call a constructor)."
  (or *%sexp-readtable*
      (setf *%sexp-readtable*
            (let ((rt (copy-readtable nil)))
              (set-dispatch-macro-character
               #\# #\S (lambda (stream char arg)
                         (declare (ignore char arg))
                         (error 'reader-error :stream stream))
               rt)
              rt))))

(defun %temporary-package ()
  (loop for name = (symbol-name (gensym "POLICY-PROTOCOL-READ-"))
        unless (find-package name)
          return (make-package name :use '("COMMON-LISP"))))

(defun %foreign-symbol (form package)
  "A symbol in FORM whose home is not PACKAGE, KEYWORD or COMMON-LISP, or NIL."
  (let ((seen (make-hash-table :test 'eq))
        (allowed (list package (find-package "KEYWORD") (find-package "COMMON-LISP"))))
    (labels ((walk (x)
               (cond ((symbolp x)
                      (let ((home (symbol-package x)))
                        (when (and home (not (member home allowed)))
                          (return-from %foreign-symbol x))))
                     ((consp x)
                      (unless (gethash x seen)
                        (setf (gethash x seen) t)
                        (walk (car x))
                        (walk (cdr x))))
                     ((and (arrayp x) (not (stringp x)))
                      (dotimes (i (array-total-size x))
                        (walk (row-major-aref x i)))))))
      (walk form)
      nil)))

(defun %read-defpolicy (text)
  "The one DEFPOLICY form in TEXT, read as data (see the file header)."
  (let ((package (%temporary-package))
        (eof '#:eof)
        (form nil)
        (problem nil))
    (unwind-protect
         (handler-case
             (with-standard-io-syntax
               (let ((*read-eval* nil)
                     (*package* package)
                     (*readtable* (%sexp-readtable)))
                 (with-input-from-string (in text)
                   (setf form (read in nil eof))
                   (cond ((eq form eof)
                          (setf problem "no form"))
                         ((not (eq (read in nil eof) eof))
                          (setf problem "more than one form"))
                         ((not (and (consp form) (symbolp (first form))
                                    (string= (symbol-name (first form)) "DEFPOLICY")))
                          (setf problem "not a (defpolicy ...) form"))
                         ((%foreign-symbol form package)
                          (setf problem (format nil "the symbol ~A belongs to another package"
                                                (symbol-name (%foreign-symbol form package)))))))))
           ((or reader-error end-of-file package-error) (c)
             (setf problem (format nil "cannot be read (~(~A~))" (type-of c)))))
      (delete-package package))
    (if problem
        (%unreadable text "~A" problem)
        form)))

;;; :json / :toml data (decoded by the settings system) → the model

(defun %data-object-p (x)
  (or (hash-table-p x)
      (and (consp x) (%proper-list-p x)
           (every (lambda (e) (and (consp e) (stringp (car e)))) x))))

(defun %data-array-p (x)
  (or (and (vectorp x) (not (stringp x)))
      (and (consp x) (%proper-list-p x) (not (%data-object-p x)))))

(defun %data-keys (x)
  (if (hash-table-p x)
      (let ((keys '()))
        (maphash (lambda (k v) (declare (ignore v)) (push k keys)) x)
        (sort keys #'string< :key #'string))
      (remove-duplicates (mapcar #'car x) :test #'equal :from-end t)))

(defun %data-get (x key)
  (if (hash-table-p x)
      (gethash key x)
      (cdr (assoc key x :test #'equal))))

(defun %data-condition (c fail)
  (cond ((%data-array-p c)
         (let ((items (coerce c 'list)))
           (unless (and items (stringp (first items))
                        (every (lambda (x) (or (stringp x) (integerp x))) items))
             (funcall fail "an atom is [REL, ARG...] of strings and integers"))
           items))
        ((%data-object-p c)
         (let ((keys (%data-keys c)))
           (unless (= 1 (length keys))
             (funcall fail "a condition object has exactly one key: not, and, or"))
           (let ((k (first keys)) (v (%data-get c (first keys))))
             (cond ((equal k "not") (list :not (%data-condition v fail)))
                   ((member k '("and" "or") :test #'equal)
                    (unless (%data-array-p v)
                      (funcall fail (format nil "~S takes an array of conditions" k)))
                    (cons (if (equal k "and") :and :or)
                          (map 'list (lambda (x) (%data-condition x fail)) v)))
                   (t (funcall fail (format nil "unknown condition operator ~S" k)))))))
        (t (funcall fail "a condition is an array (atom) or an object (not / and / or)"))))

(defun %data-resource (r fail)
  (cond ((stringp r) r)
        ((%data-object-p r)
         (let ((keys (%data-keys r)))
           (unless (= 1 (length keys))
             (funcall fail "a resource object has exactly one of glob, prefix, path, host"))
           (let ((kind (cdr (assoc (first keys) '(("glob" . :glob) ("prefix" . :prefix)
                                                  ("path" . :path) ("host" . :host))
                                   :test #'equal)))
                 (v (%data-get r (first keys))))
             (unless (and kind (stringp v))
               (funcall fail "a resource object is {\"glob\" | \"prefix\" | \"path\" | \"host\": STRING}"))
             (list kind v))))
        (t (funcall fail "a resource is a string or an object"))))

(defun %data-rule-form (data)
  "DATA (one rule object) as a DSL rule form, keys in the canonical order."
  (flet ((fail (reason) (%syntax-error data "~A" reason)))
    (let ((effect nil) (id nil) (action nil) (resource nil) (when nil)
          (obligations nil) (feedback nil))
      (dolist (key (%data-keys data))
        (let ((v (%data-get data key)))
          (flet ((string-value ()
                   (if (stringp v) v (fail (format nil "~S must be a string" key)))))
            (cond ((equal key "effect")
                   (setf effect (or (cdr (assoc v '(("permit" . permit) ("ask" . ask)
                                                    ("forbid" . forbid))
                                                :test #'equal))
                                    (fail (format nil "unknown effect ~S" v)))))
                  ((equal key "id") (setf id (list :id (string-value))))
                  ((equal key "action") (setf action (list :action (string-value))))
                  ((equal key "resource")
                   (setf resource (list :resource (%data-resource v #'fail))))
                  ((equal key "when")
                   (setf when (list :when (%data-condition v #'fail))))
                  ((equal key "obligations")
                   (unless (%data-array-p v) (fail "\"obligations\" must be an array"))
                   (setf obligations
                         (list :obligations
                               (map 'list (lambda (o)
                                            (unless (and (%data-array-p o)
                                                         (every (lambda (x) (or (stringp x) (integerp x)))
                                                                (coerce o 'list)))
                                              (fail "an obligation is [KIND, ARG...]"))
                                            (coerce o 'list))
                                    v))))
                  ((equal key "feedback") (setf feedback (list :feedback (string-value))))
                  (t (fail (format nil "unknown rule key ~S" key)))))))
      (unless effect (fail "\"effect\" is required"))
      (append (list effect) id action resource when obligations feedback))))

(defun %data-rule-with-restarts (data)
  "One data rule with SKIP-RULE and USE-VALUE (a replacement: a rule object,
a DSL rule form or a POLICY-RULE, checked again). → a parse or :SKIP."
  (loop
    (restart-case (return (%parse-rule (if (%data-object-p data) (%data-rule-form data) data)))
      (skip-rule ()
        :report "Leave this rule out and go on with the rest."
        (return :skip))
      (use-value (value)
        :report "Use another rule in its place."
        (setf data value)))))

(defun %data-option (value check)
  "%CHECKED-OPTION for data: CHECK sees the value; USE-VALUE replaces it."
  (%checked-option value check))

(defun %policy-from-data (data text)
  "The POLICY for DATA, a decoded policy object (hash tables or alists with
string keys, vectors or lists for arrays). TEXT is the source, for errors."
  (unless (%data-object-p data)
    (%unreadable text "the policy data is not an object"))
  (let ((name nil) (combining :forbid-overrides) (default nil) (rules '()))
    (dolist (key (%data-keys data))
      (let ((v (%data-get data key)))
        (cond ((equal key "name")
               (setf name (%data-option v (lambda (v)
                                            (if (or (stringp v) (null v))
                                                v
                                                (%syntax-error v "\"name\" must be a string"))))))
              ((equal key "combining")
               (setf combining
                     (%data-option v (lambda (v)
                                       (%check-combining
                                        (or (cdr (assoc v '(("forbid-overrides" . :forbid-overrides)
                                                            ("last-match" . :last-match))
                                                        :test #'equal))
                                            v))))))
              ((equal key "default")
               (setf default
                     (%data-option v (lambda (v)
                                       (%check-default
                                        (or (cdr (assoc v '(("permit" . :permit) ("ask" . :ask)
                                                            ("forbid" . :forbid))
                                                        :test #'equal))
                                            v))))))
              ((equal key "rules")
               (setf rules (%data-option v (lambda (v)
                                             (if (%data-array-p v)
                                                 (coerce v 'list)
                                                 (%syntax-error v "\"rules\" must be an array"))))))
              (t (restart-case (%syntax-error key "unknown policy key ~S" key)
                   (use-value (value)
                     :report "Ignore this key."
                     (declare (ignore value))))))))
    (%build-policy name combining default rules :parse #'%data-rule-with-restarts)))

(defun load-policy (source &key format)
  "The POLICY in SOURCE (see the file header)."
  (multiple-value-bind (text source-format) (%source-text source)
    (let ((format (or format source-format :sexp)))
      (%with-source-restart
        (if (eq format :sexp)
            (parse-policy (%read-defpolicy text))
            (decode-policy-data format text))))))

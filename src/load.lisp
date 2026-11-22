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

(defun load-policy (source &key format)
  "The POLICY in SOURCE (see the file header)."
  (declare (ignore source format))
  (%not-implemented 'load-policy))

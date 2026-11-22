(in-package #:policy-protocol)

;;; policy-protocol/settings: policies as JSON / TOML data (shape:
;;; load.lisp). Decoding goes through json-protocol and toml-protocol (the
;;; caller loads a backend, e.g. json-backend-jzon, toml-backend-tomlet);
;;; this system loads no backend itself.

(defmethod decode-policy-data ((format (eql :json)) text)
  (%with-source-restart
    (let ((data (handler-case (json-protocol:decode text)
                  (json-protocol:json-error (c)
                    (%unreadable text "not JSON (~A)" (json-protocol:json-error-message c))))))
      (%policy-from-data data text))))

(defmethod decode-policy-data ((format (eql :toml)) text)
  (%with-source-restart
    (let ((data (handler-case (toml-protocol:decode text)
                  (toml-protocol:toml-error (c)
                    (%unreadable text "not TOML (~A)" (toml-protocol:toml-error-message c))))))
      (%policy-from-data data text))))

;;; Writing. json-protocol encodes an alist as an object in alist order and a
;;; vector as an array, so every object below is an alist and every array a
;;; vector (a list of conses could read as an object, NIL as false).

(defun %json-condition (c)
  (case (and (keywordp (first c)) (first c))
    (:not (list (cons "not" (%json-condition (second c)))))
    ((:and :or) (list (cons (string-downcase (symbol-name (first c)))
                            (map 'vector #'%json-condition (rest c)))))
    (t (coerce c 'vector))))

(defun %json-resource (r)
  (if (stringp r)
      r
      (list (cons (string-downcase (symbol-name (first r))) (second r)))))

(defun %json-rule (form)
  (destructuring-bind (&key id effect action resource when obligations feedback) form
    (append (list (cons "effect" (string-downcase (symbol-name effect)))
                  (cons "id" id)
                  (cons "action" action)
                  (cons "resource" (%json-resource resource)))
            (and when (list (cons "when" (%json-condition when))))
            (and obligations
                 (list (cons "obligations" (map 'vector (lambda (o) (coerce o 'vector)) obligations))))
            (and feedback (list (cons "feedback" feedback))))))

(defun policy-to-json (policy)
  "POLICY (a POLICY) as JSON text in LOAD-POLICY's shape (load.lisp),
deterministic: LOAD-POLICY of the result gives an EQUAL POLICY-FORM."
  (destructuring-bind (&key name combining default rules) (policy-form policy)
    (json-protocol:encode
     (append (and name (list (cons "name" name)))
             (list (cons "combining" (string-downcase (symbol-name combining))))
             (and default (list (cons "default" (string-downcase (symbol-name default)))))
             (list (cons "rules" (map 'vector #'%json-rule rules)))))))

(in-package #:policy-protocol)

;;; policy-protocol/settings: policies as JSON / TOML data (shape:
;;; load.lisp). Decoding goes through json-protocol and toml-protocol (the
;;; caller loads a backend, e.g. json-backend-jzon, toml-backend-tomlet);
;;; this system loads no backend itself.

(defmethod decode-policy-data ((format (eql :json)) text)
  (declare (ignore text))
  (%not-implemented 'decode-policy-data))

(defmethod decode-policy-data ((format (eql :toml)) text)
  (declare (ignore text))
  (%not-implemented 'decode-policy-data))

(defun policy-to-json (policy)
  "POLICY (a POLICY) as JSON text in LOAD-POLICY's shape (load.lisp),
deterministic: LOAD-POLICY of the result gives an EQUAL POLICY-FORM."
  (declare (ignore policy))
  (%not-implemented 'policy-to-json))

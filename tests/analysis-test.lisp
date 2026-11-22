(in-package #:policy-protocol/tests)

;;; The analysis GFs are declared in 0.1.0; their methods come with the SAT
;;; bridge in 0.2.0 (L2). Until then they signal POLICY-ANALYSIS-UNAVAILABLE
;;; with USE-VALUE. A placeholder object stands in for the policy: the
;;; default methods never look at it.

(deftest analysis-needs-a-backend
  (let ((p (make-instance 'policy :name "placeholder")))
    (dolist (call (list (list 'policy-conflicts (lambda () (policy-conflicts p)))
                        (list 'policy-unreachable-rules (lambda () (policy-unreachable-rules p)))
                        (list 'policy-subsumes-p (lambda () (policy-subsumes-p p p)))
                        (list 'policy-equivalent-p (lambda () (policy-equivalent-p p p)))))
      (destructuring-bind (name fn) call
        (ok (signals (funcall fn) 'policy-analysis-unavailable) (string name))
        (ok (eq name (handler-case (funcall fn)
                       (policy-analysis-unavailable (c) (policy-error-operation c))))
            (format nil "~A: the condition names the operation" name))
        (ok (typep (handler-case (funcall fn) (policy-error (c) c)) 'policy-error))
        (ok (eq :answer (handler-bind ((policy-analysis-unavailable
                                         (lambda (c) (declare (ignore c)) (use-value :answer))))
                          (funcall fn)))
            (format nil "~A: USE-VALUE supplies the result" name))))))

(deftest analysis-loads-no-solver
  (ng (find-package "SAT-PROTOCOL") "evaluating policies never loads a SAT solver"))

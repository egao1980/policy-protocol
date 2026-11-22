(in-package #:policy-protocol/tests)

;;; Combining within a layer (:forbid-overrides, :last-match), defaults, and
;;; layers that only tighten (model.lisp: ONE POLICY, ONE REQUEST; LAYERS).

(defparameter *mixed-rules*
  '((permit :id "p" :action "edit" :resource "src/*")
    (ask    :id "a" :action "edit" :resource "src/gen/*")
    (forbid :id "f" :action "edit" :resource "src/gen/secret*")))

(deftest forbid-overrides-is-the-default
  (ok (eq :forbid-overrides (policy-combining (make-policy :rules '((permit :action "x"))))))
  (ok (eq :forbid-overrides (policy-combining (plan-mode)))))

(deftest-parametrize forbid-overrides-effects
    ((resources effect id)
     ('("src/a.lisp") :permit "p")
     ('("src/gen/x.lisp") :ask "a")
     ('("src/gen/secret.key") :forbid "f")
     ('("src/a.lisp" "src/gen/x.lisp") :ask "a")
     ('("src/gen/x.lisp" "src/gen/secret.key") :forbid "f")
     ('("lib/a.lisp") :no-decision nil)
     ('("src/a.lisp" "lib/a.lisp") :no-decision nil)
     ('("src/gen/x.lisp" "lib/a.lisp") :ask "a"))
  (let ((d (decide (apply #'policy-of :forbid-overrides *mixed-rules*) (req "edit" resources))))
    (ok (eq (decision-effect d) effect))
    (ok (equal (rule-id d) id))))

(deftest forbid-overrides-ignores-rule-order
  (dolist (resources '(("src/a.lisp") ("src/gen/x.lisp") ("src/gen/secret.key")
                       ("src/a.lisp" "src/gen/secret.key")))
    (ok (eq (effect (apply #'policy-of :forbid-overrides *mixed-rules*) (req "edit" resources))
            (effect (apply #'policy-of :forbid-overrides (reverse *mixed-rules*))
                    (req "edit" resources)))
        (format nil "~S" resources))))

(deftest forbid-overrides-deciding-rule-and-resource
  (let ((d (decide (policy-of :forbid-overrides
                              '(forbid :id "f1" :action "edit" :resource "*.key")
                              '(forbid :id "f2" :action "edit" :resource "secret/*"))
                   (req "edit" '("src/a" "secret/b.key" "x.key")))))
    (ok (eq :forbid (decision-effect d)))
    (ok (equal "f1" (rule-id d)) "the first rule in policy order with the effect")
    (ok (equal "secret/b.key" (decision-resource d)) "the first resource in request order")))

(deftest last-match-lets-a-later-rule-loosen
  (let ((forbid-then-permit '((forbid :id "f" :action "bash" :resource "*")
                              (permit :id "p" :action "bash" :resource "git *"))))
    (ok (eq :permit (effect (apply #'policy-of :last-match forbid-then-permit)
                            (req "bash" "git log"))))
    (ok (eq :forbid (effect (apply #'policy-of :forbid-overrides forbid-then-permit)
                            (req "bash" "git log")))
        "the same rules under :forbid-overrides")
    (ok (eq :forbid (effect (apply #'policy-of :last-match (reverse forbid-then-permit))
                            (req "bash" "git log")))
        "order matters under :last-match")))

(deftest default-effect
  (let ((p (make-policy :default :forbid :rules '((permit :id "p" :action "read")))))
    (let ((d (decide p (req "write" "x"))))
      (ok (eq :forbid (decision-effect d)))
      (ok (decision-default-p d))
      (ok (null (decision-rule d)))
      (ok (null (decision-obligations d))))
    (let ((d (decide p (req "read" "x"))))
      (ok (eq :permit (decision-effect d)))
      (ng (decision-default-p d))))
  (testing "(default EFFECT) in a DEFPOLICY body"
    (let ((p (parse-policy '(defpolicy closed () (permit :action "read") (default :forbid)))))
      (ok (eq :forbid (policy-default p)))
      (ok (eq :forbid (effect p (req "write" "x"))))))
  (testing "no default: no decision, the caller's default applies"
    (let ((d (decide (policy-of :forbid-overrides '(permit :action "read")) (req "write" "x"))))
      (ok (eq :no-decision (decision-effect d)))
      (ng (decision-default-p d)))))

(defun layer (name effect &rest keys)
  (apply #'make-policy :name name
                       :rules (list `(,effect :id ,(format nil "~A-~(~A~)" name effect)
                                              :action "edit"))
                       keys))

(defun none (name)
  (make-policy :name name :rules '((permit :action "never-called"))))

(deftest-parametrize layers-only-tighten
    ((outer inner effect deciding-layer)
     ;; the brief's two cases
     (:forbid :permit :forbid "outer")
     (:permit :forbid :forbid "inner")
     (:permit :ask :ask "inner")
     (:ask :permit :ask "outer")
     (:ask :forbid :forbid "inner")
     (:forbid :ask :forbid "outer")
     (:permit :permit :permit "outer")
     (:forbid :forbid :forbid "outer")
     ;; an inner permit cannot turn the outer's no decision into a permit
     (nil :permit :no-decision nil)
     (nil :ask :ask "inner")
     (nil :forbid :forbid "inner")
     (:permit nil :permit "outer")
     (nil nil :no-decision nil))
  (let* ((o (if outer (layer "outer" outer) (none "outer")))
         (i (if inner (layer "inner" inner) (none "inner")))
         (d (decide (make-layered-policy o i) (req "edit" "x"))))
    (ok (eq (decision-effect d) effect))
    (ok (equal (and (decision-layer d) (policy-name (decision-layer d))) deciding-layer))
    (ok (equal (mapcar (lambda (e) (policy-name (trace-policy e))) (decision-trace d))
               '("outer" "inner"))
        "one trace entry per layer, outer first")))

(deftest layer-defaults-count-as-the-layer-effect
  (let ((d (decide (make-layered-policy (make-policy :name "outer" :default :forbid
                                                     :rules '((permit :action "read")))
                                        (layer "inner" 'permit))
                   (req "edit" "x"))))
    (ok (eq :forbid (decision-effect d)))
    (ok (decision-default-p d))))

(deftest layered-policies-flatten
  (let* ((a (layer "a" 'permit))
         (b (layer "b" 'ask))
         (c (layer "c" 'permit))
         (lp (make-layered-policy (make-layered-policy a b) c)))
    (ok (equal (mapcar #'policy-name (layered-policy-layers lp)) '("a" "b" "c")))
    (ok (eq :ask (effect lp (req "edit" "x"))))))

(deftest layers-by-name
  (defpolicy policy-test-outer-layer () (permit :id "o" :action "edit"))
  (defpolicy policy-test-inner-layer () (ask :id "i" :action "edit"))
  (let ((d (decide (make-layered-policy 'policy-test-outer-layer 'policy-test-inner-layer)
                   (req "edit" "x"))))
    (ok (eq :ask (decision-effect d)))
    (ok (equal "i" (rule-id d))))
  (ok (eq :ask (effect 'policy-test-inner-layer (req "edit" "x"))) "a symbol names a policy"))

(deftest mixed-combining-across-layers
  (let ((outer (make-policy :name "user" :combining :last-match
                            :rules '((forbid :id "u0" :action "bash")
                                     (permit :id "u1" :action "bash" :resource "git *"))))
        (inner (make-policy :name "project"
                            :rules '((permit :id "j0" :action "bash")
                                     (forbid :id "j1" :action "bash" :resource "git push*")))))
    (ok (eq :permit (effect (make-layered-policy outer inner) (req "bash" "git log"))))
    (ok (eq :forbid (effect (make-layered-policy outer inner) (req "bash" "git push"))))
    (ok (eq :forbid (effect (make-layered-policy outer inner) (req "bash" "rm -rf x")))
        "the project's permit cannot widen the user's forbid")))

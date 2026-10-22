(in-package #:http2-spec)

;;; A derived spec shares every declaration of its base except the ones it replaces, and gets
;;; its own module + instance. It is registered for the spec backends only (no CLOS expansion),
;;; so the base stays the single CLOS protocol and a variant cannot drift from it silently.

(defun derive-spec (base name &key module instance-name bindings decls)
  "Register NAME as a Quint-only variant of spec BASE. MODULE and INSTANCE-NAME name the
emitted modules; BINDINGS ((const-symbol DSL-form) ...) override the base instance bindings;
DECLS are DSL declaration forms that replace same-named base declarations. Returns the module."
  (let* ((base (spec-protocol:find-spec base))
         (env (spec-protocol:module-env base))
         (base-instance (spec-protocol:module-instance base))
         (instance-bindings
           (loop for (sym . expr) in (and base-instance (spec-protocol:instance-bindings base-instance))
                 for override = (assoc sym bindings :test #'spec-protocol:name=)
                 collect (cons sym (if override (spec-protocol:parse-expr (second override) env) expr))))
         (module (make-instance 'spec-protocol:spec-module
                                :lisp-name name
                                :name module
                                :doc (spec-protocol:module-doc base)
                                :options (list* :clos nil (spec-protocol:module-options base))
                                :instance (make-instance 'spec-protocol:instance-decl
                                                         :name instance-name
                                                         :bindings instance-bindings)
                                :decls (copy-list (spec-protocol:module-decls base)))))
    (dolist (form decls)
      (let ((spec-only (and (consp form) (eq (car form) :spec-only))))
        (spec-protocol:module-add-decl
         module (spec-protocol:parse-decl (if spec-only (second form) form) env spec-only))))
    (spec-protocol:register-spec module)))

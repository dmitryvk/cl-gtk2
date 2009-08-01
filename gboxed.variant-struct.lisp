(in-package :gobject)

(defstruct var-structure
  name
  parent
  slots
  discriminator-slot
  variants)

(defstruct var-structure-variant
  discriminating-values
  structure)

(defstruct var-structure-slot
  name
  type
  initform
  count)

(defmethod make-load-form ((object var-structure) &optional env)
  (make-load-form-saving-slots object :environment env))

(defmethod make-load-form ((object var-structure-slot) &optional env)
  (make-load-form-saving-slots object :environment env))

(defmethod make-load-form ((object var-structure-variant) &optional env)
  (make-load-form-saving-slots object :environment env))

(defun var-struct-all-slots (struct)
  (when struct
    (append (var-struct-all-slots (var-structure-parent struct))
            (var-structure-slots struct))))

(defun all-structures (structure)
  (append (iter (for variant in (var-structure-variants structure))
                (appending (all-structures (var-structure-variant-structure variant))))
          (list structure)))

(defun parse-variant-structure-definition (name slots &optional parent)
  (iter (with result = (make-var-structure :name name
                                           :parent parent
                                           :slots nil
                                           :discriminator-slot nil
                                           :variants nil))
        (for slot in slots)
        (if (eq :variant (first slot))
            (progn
              (when (var-structure-discriminator-slot result)
                (error "Structure has more than one discriminator slot"))
              (setf (var-structure-discriminator-slot result) (second slot)
                    (var-structure-variants result) (parse-variants result (nthcdr 2 slot))))
            (push (parse-slot slot) (var-structure-slots result)))
        (finally (setf (var-structure-slots result)
                       (reverse (var-structure-slots result)))
                 (return result))))

(defun parse-slot (slot)
  (destructuring-bind (name type &key count initform) slot
    (make-var-structure-slot :name name :type type :count count :initform initform)))

(defun parse-variants (parent variants)
  (iter (for var-descr in variants)
        (for (options variant-name . slots) in variants)
        (for variant =
             (make-var-structure-variant
              :discriminating-values (ensure-list options)
              :structure (parse-variant-structure-definition variant-name slots parent)))
        (collect variant)))




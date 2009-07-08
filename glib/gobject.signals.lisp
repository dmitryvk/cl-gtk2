(in-package :gobject)

(defcfun g-signal-connect-closure :ulong
  (instance :pointer)
  (detailed-signal :string)
  (closure (:pointer g-closure))
  (after :boolean))

(defcfun g-signal-emitv :void
  (instance-and-params (:pointer g-value))
  (signal-id :uint)
  (detail g-quark)
  (return-value (:pointer g-value)))

(defcfun g-signal-lookup :uint
  (name :string)
  (type g-type))

(defbitfield g-signal-flags
  :run-first :run-last :run-cleanup :no-recurse :detailed :action :no-hooks)

(defcstruct g-signal-query
  (signal-id :uint)
  (signal-name :string)
  (owner-type g-type)
  (signal-flags g-signal-flags)
  (return-type g-type)
  (n-params :uint)
  (param-types (:pointer g-type)))

(defcfun g-signal-query :void
  (signal-id :uint)
  (query (:pointer g-signal-query)))

(defun unmangle-type (type)
  (logxor type (ldb (byte 1 0) type)));;subtract the G_SIGNAL_TYPE_STATIC_SCOPE

(defun emit-signal (object signal-name &rest args)
  "Emits the signal.
@arg[object]{an instance of @class{g-object}. Signal is emitted on this object}
@arg[signal-name]{a string specifying the signal}
@arg[args]{arguments for the signal}
@return{none}"
  (let ((signal-id (g-signal-lookup signal-name (g-type-from-object (pointer object)))))
    (when (= signal-id 0)
      (error "Signal ~A not found on object ~A" signal-name object))
    (with-foreign-object (q 'g-signal-query)
      (g-signal-query signal-id q)
      (with-foreign-object (params 'g-value (+ 1 (foreign-slot-value q 'g-signal-query 'n-params)))
        (set-g-value (mem-aref params 'g-value 0) object (g-type-from-object (pointer object)) :zero-g-value t)
        (iter (for i from 0 below (foreign-slot-value q 'g-signal-query 'n-params))
              (for arg in args)
              (for type = (unmangle-type (mem-aref (foreign-slot-value q 'g-signal-query 'param-types) 'g-type i)))
              (set-g-value (mem-aref params 'g-value (1+ i)) arg type :zero-g-value t))
        (prog1
            (if (= (foreign-slot-value q 'g-signal-query 'return-type) +g-type-void+)
                (g-signal-emitv params signal-id signal-name (null-pointer))
                (with-foreign-object (return-value 'g-value)
                  (g-value-zero return-value)
                  (g-value-init return-value (foreign-slot-value q 'g-signal-query 'return-type))
                  (prog1 (parse-gvalue return-value)
                    (g-value-unset return-value))))
          (iter (for i from 0 below (foreign-slot-value q 'g-signal-query 'n-params))
                (g-value-unset (mem-aref params 'g-value (1+ i)))))))))
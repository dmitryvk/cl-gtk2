(in-package :gobject)

;;; Signal handler closures

(defcstruct lisp-signal-handler-closure
  (:parent-instance g-closure)
  (:object :pointer)
  (:function-id :int))

(defun finalize-lisp-signal-handler-closure (closure)
  (let* ((function-id (foreign-slot-value closure 'lisp-signal-handler-closure :function-id))
         (addr (pointer-address (foreign-slot-value closure 'lisp-signal-handler-closure :object)))
         (object (or (gethash addr *foreign-gobjects-strong*)
                     (gethash addr *foreign-gobjects-weak*))))
    (when object
      (delete-handler-from-object object function-id))))

(defcallback lisp-signal-handler-closure-finalize :void
    ((data :pointer) (closure (:pointer lisp-signal-handler-closure)))
  (declare (ignore data))
  (finalize-lisp-signal-handler-closure closure))

(defun call-with-restarts (fn args)
  (restart-case
      (apply fn args)
    (return-from-g-closure (&optional v) :report "Return value from closure" v)))

(defcallback lisp-signal-handler-closure-marshal :void
    ((closure (:pointer lisp-signal-handler-closure))
     (return-value (:pointer g-value))
     (count-of-args :uint)
     (args (:pointer g-value))
     (invocation-hint :pointer)
     (marshal-data :pointer))
  (declare (ignore invocation-hint marshal-data))
  (let* ((args (parse-closure-arguments count-of-args args))
         (function-id (foreign-slot-value closure 'lisp-signal-handler-closure :function-id))
         (addr (pointer-address (foreign-slot-value closure 'lisp-signal-handler-closure :object)))
         (object (or (gethash addr *foreign-gobjects-strong*)
                     (gethash addr *foreign-gobjects-weak*)))
         (return-type (and (not (null-pointer-p return-value))
                           (g-value-type return-value)))
         (fn (retrieve-handler-from-object object function-id))
         (fn-result (call-with-restarts fn args)))
    (when return-type
      (set-g-value return-value fn-result return-type :g-value-init nil))))

(defun parse-closure-arguments (count-of-args args)
  (loop
     for i from 0 below count-of-args
     collect (parse-g-value (mem-aref args 'g-value i))))

(defun create-signal-handler-closure (object fn)
  (let ((function-id (save-handler-to-object object fn))
        (closure (g-closure-new-simple (foreign-type-size 'lisp-signal-handler-closure) (null-pointer))))
    (setf (foreign-slot-value closure 'lisp-signal-handler-closure :function-id) function-id
          (foreign-slot-value closure 'lisp-signal-handler-closure :object) (pointer object))
    (g-closure-add-finalize-notifier closure (null-pointer)
                                     (callback lisp-signal-handler-closure-finalize))
    (g-closure-set-marshal closure (callback lisp-signal-handler-closure-marshal))
    closure))

(defun find-free-signal-handler-id (object)
  (iter (with handlers = (g-object-signal-handlers object))
        (for i from 0 below (length handlers))
        (finding i such-that (null (aref handlers i)))))

(defun save-handler-to-object (object handler)
  (assert handler)
  (let ((id (find-free-signal-handler-id object))
        (handlers (g-object-signal-handlers object)))
    (if id
        (progn (setf (aref handlers id) handler) id)
        (progn (vector-push-extend handler handlers) (1- (length handlers))))))

(defun retrieve-handler-from-object (object handler-id)
  (aref (g-object-signal-handlers object) handler-id))

(defun delete-handler-from-object (object handler-id)
  (let ((handlers (g-object-signal-handlers object)))
    (setf (aref handlers handler-id) nil)
    (iter (while (plusp (length handlers)))
          (while (null (aref handlers (1- (length handlers)))))
          (vector-pop handlers))
    nil))

(defun connect-signal (object signal handler &key after)
  "Connects the function to a signal for a particular object.
If @code{after} is true, then the function will be called after the default handler of the signal.

@arg[object]{an instance of @class{gobject}}
@arg[signal]{a string; names the signal}
@arg[handler]{a function; handles the signal. Number (and type) of arguments and return value type depends on the signal}
@arg[after]{a boolean}"
  (g-signal-connect-closure (pointer object)
                            signal
                            (create-signal-handler-closure object handler)
                            after))

(defun g-signal-connect (object signal handler &key after)
  "Deprecated alias for @fun{connect-signal}"
  (connect-signal object signal handler :after after))

(defun emit-signal (object signal-name &rest args)
  "Emits the signal.
@arg[object]{an instance of @class{g-object}. Signal is emitted on this object}
@arg[signal-name]{a string specifying the signal}
@arg[args]{arguments for the signal}
@return{none}"
  (let* ((object-type (g-type-from-object (pointer object)))
         (signal-info (parse-signal-name object-type signal-name)))
    (unless signal-info
      (error "Signal ~A not found on object ~A" signal-name object))
    (let ((params-count (length (signal-info-param-types signal-info))))
      (with-foreign-object (params 'g-value (1+ params-count))
        (set-g-value (mem-aref params 'g-value 0) object object-type :zero-g-value t)
        (iter (for i from 0 below params-count)
              (for arg in args)
              (for type in (signal-info-param-types signal-info))
              (set-g-value (mem-aref params 'g-value (1+ i)) arg type :zero-g-value t))
        (prog1
            (if (g-type= (signal-info-return-type signal-info) +g-type-void+)
                (g-signal-emitv params (signal-info-id signal-info) signal-name (null-pointer))
                (with-foreign-object (return-value 'g-value)
                  (g-value-zero return-value)
                  (g-value-init return-value (signal-info-return-type signal-info))
                  (prog1 (parse-g-value return-value)
                    (g-value-unset return-value))))
          (iter (for i from 0 below (1+ params-count))
                (g-value-unset (mem-aref params 'g-value i))))))))

(defcfun (disconnect-signal "g_signal_handler_disconnect") :void
  (object g-object)
  (handler-id :ulong))

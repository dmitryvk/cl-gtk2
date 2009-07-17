(in-package :gobject)

(defstruct signal-info
  id
  name
  owner-type
  flags
  return-type
  param-types
  detail)

(defmethod print-object ((instance signal-info) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (instance stream)
        (format stream
                "Signal [#~A] ~A ~A.~A~@[::~A~](~{~A~^, ~})~@[ [~{~A~^, ~}]~]"
                (signal-info-id instance)
                (g-type-string (signal-info-return-type instance))
                (g-type-string (signal-info-owner-type instance))
                (signal-info-name instance)
                (signal-info-detail instance)
                (mapcar #'g-type-string (signal-info-param-types instance))
                (signal-info-flags instance)))))

(defun query-signal-info (signal-id)
  (with-foreign-object (q 'g-signal-query)
    (g-signal-query signal-id q)
    (assert (not (zerop (foreign-slot-value q 'g-signal-query :signal-id))))
    (let ((param-types
           (iter (with param-types = (foreign-slot-value q 'g-signal-query :param-types))
                 (for i from 0 below (foreign-slot-value q 'g-signal-query :n-params))
                 (for param-type = (mem-aref param-types '(g-type-designator :mangled-p t) i))
                 (collect param-type))))
      (make-signal-info :id signal-id
                        :name (foreign-slot-value q 'g-signal-query :signal-name)
                        :owner-type (foreign-slot-value q 'g-signal-query :owner-type)
                        :flags (foreign-slot-value q 'g-signal-query :signal-flags)
                        :return-type (foreign-slot-value q 'g-signal-query :return-type)
                        :param-types param-types))))

(defun parse-signal-name (owner-type signal-name)
  (with-foreign-objects ((signal-id :uint) (detail 'glib:g-quark))
    (when (g-signal-parse-name signal-name owner-type signal-id detail t)
      (let ((signal-info (query-signal-info (mem-ref signal-id :uint))))
        (setf (signal-info-detail signal-info) (mem-ref detail 'g-quark))
        signal-info))))

(defun type-signals (type &key include-inherited)
  (unless (g-type= type +g-type-invalid+)
    (let ((signals (with-foreign-object (n-ids :uint)
                     (with-unwind (ids (g-signal-list-ids type n-ids) g-free)
                       (iter (for i from 0 below (mem-ref n-ids :uint))
                             (collect (query-signal-info (mem-aref ids :uint i))))))))
      (if include-inherited
          (nconc (type-signals (g-type-parent type) :include-inherited t)
                 (iter (for interface in (g-type-interfaces type))
                       (nconcing (type-signals interface :include-inherited t)))
                 signals)
          signals))))

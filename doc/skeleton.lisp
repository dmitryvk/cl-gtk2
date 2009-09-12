(defpackage :doc-skeleton
  (:use :cl :gtk :gdk :gobject :iter :c2mop :glib)
  (:export :widget-skeleton
           :widgets-chapter-skeleton
           :enum-skeleton
           :flags-skeleton
           :all-enums
           :all-flags
           :enum-chapter-skeleton
           :flags-chapter-skeleton
           :struct-skeleton
           #:all-structs
           #:struct-chapter-skeleton
           #:interface-chapter-skeleton
           #:all-interfaces
           #:interface-skeleton
           #:all-widgets
           #:all-classes
           #:all-gtk-skeletons))

(in-package :doc-skeleton)

(defun widgets-chapter-skeleton (output widgets &key (use-refs t) (section "section"))
  (cond
    ((or (pathnamep output)
         (stringp output))
     (with-open-file (stream output :direction :output :if-exists :supersede)
       (widgets-chapter-skeleton stream widgets :use-refs use-refs :section section)))
    ((null output) (with-output-to-string (stream)
                     (widgets-chapter-skeleton stream widgets :use-refs use-refs :section section)))
    ((or (eq t output) (streamp output))
     (format output "@menu~%")
     (iter (for w in widgets)
           (format output "* ~A::~%" (string-downcase (symbol-name w))))
     (format output "@end menu~%~%")
     (iter (for w in widgets)
           (write-string (widget-skeleton w :section section :use-refs use-refs) output)
           (format output "~%~%")))))

(defparameter *exclusions* '(gdk:display gdk:screen))

(defparameter *ref-exclusions* '(gtk-demo::custom-window gtkglext:gl-drawing-area gtkglext:gdk-gl-window gtkglext:gdk-gl-pixmap))

(defun all-widgets (package)
  (sort (iter (for symbol in-package (find-package package) :external-only t)
              (unless (member symbol *exclusions*)
                (for class = (find-class symbol nil))
                (when (and class (subclassp class (find-class 'gtk:widget)))
                  (collect symbol))))
        #'string<))

(defun all-classes (package)
  (sort (iter (for symbol in-package (find-package package) :external-only t)
              (unless (member symbol *exclusions*)
                (for class = (find-class symbol nil))
                (when (and class
                           (not (subclassp class (find-class 'condition)))
                           (not (subclassp class (find-class 'gtk:widget)))
                           (or (not (typep class 'gobject::gobject-class))
                               (not (gobject::gobject-class-interface-p class)))
                           (not (typep class 'structure-class)))
                  (collect symbol))))
        #'string<))

;; (widget-skeleton widget &key (sectioning-command "section"))
;; returns the texinfo string for widget (a symbol or class)
;; Template:
;; 
;; @node $WIDGET
;; @$SECTIONING-COMMAND $WIDGET
;;
;; @Class $WIDGET
;; 
;; Superclass: $(direct-superclass WIDGET)
;;
;; Interfaces: $(direct-interface widget)
;;
;; Slots:
;; @itemize
;; $(for each slot)
;; @item @anchor{slot.$widget.$slot}$slot. Type: $(slot-type slot). Accessor: $(slot-accessor slot). $(when (constructor-only slot) "Contructor-only slot.")
;; $(end for)
;; @end itemize
;;
;; Signals:
;; @itemize
;; $(for each signal)
;; @item @anchor{signal.$widget.$signal}"$signal". Signature: Type1 Arg1, .., Typen Argn => return-type. Options: $(signal-options)
;; $(end for)
;; @end itemize

(defvar *use-refs* t)

(defun widget-skeleton (widget &key (section "section") (use-refs t))
  (unless (typep widget 'class) (setf widget (find-class widget)))
  (with-output-to-string (stream)
    (let ((*print-case* :downcase)
          (*package* (symbol-package (class-name widget)))
          (*print-circle* nil)
          (*use-refs* use-refs))
      (format stream "@node ~A~%" (class-name widget))
      (format stream "@~A ~A~%" section (class-name widget))
      (format stream "@Class ~A~%" (class-name widget))
      (format stream "Superclass:")
      (iter (for super in (class-direct-superclasses widget))
            (format stream " ~A" (format-ref (class-name super))))
      (when (class-direct-subclasses widget)
        (format stream "~%~%")
        (format stream "Subclasses:")
        (iter (for sub in (class-direct-subclasses widget))
              (unless (member (class-name sub) *ref-exclusions*)
                (format stream " ~A" (format-ref (class-name sub))))))
      (format stream "~%~%")
      (widget-slots stream widget)
      (format stream "~%~%")
      (widget-signals stream widget)
      (format stream "~%~%")
      (widget-child-properties stream widget))))

(defun widget-slots (stream widget)
  (format stream "Slots:~%")
  (format stream "@itemize~%")
  (iter (for slot in (sort (copy-list (class-direct-slots widget)) #'string< :key #'slot-definition-name))
        (when (typep slot 'gobject::gobject-direct-slot-definition)
          (format stream "@item @anchor{slot.~A.~A}~A. Type: ~A. Accessor: ~A."
                  (class-name widget) (slot-definition-name slot)
                  (slot-definition-name slot)
                  (slot-type slot)
                  (slot-accessor slot))
          (case (classify-slot-readability widget slot)
            (:write-only (format stream " Write-only."))
            (:read-only (format stream " Read-only.")))
          (format stream "~%")))
  (format stream "@end itemize~%"))

(defun widget-signals (stream widget)
  (when (typep widget 'gobject::gobject-class)
    (let ((g-type (gobject::gobject-class-direct-g-type-name widget)))
      (when g-type
        (format stream "Signals:~%")
        (format stream "@itemize~%")
        ;; @item @anchor{signal.$widget.$signal}"$signal". Signature: Type1 Arg1, .., Typen Argn => return-type. Options: $(signal-options)
        (iter (for signal in (sort (copy-list (type-signals g-type)) #'string< :key #'signal-info-name))
              (format stream "@item @anchor{signal.~A.~A}\"~A\". Signature: ~A. Options: ~A."
                      (class-name widget)
                      (signal-info-name signal)
                      (signal-info-name signal)
                      (signal-signature signal)
                      (signal-options signal))
              (format stream "~%"))
        (format stream "@end itemize~%")))))

(defun widget-child-properties (stream widget)
  (when (typep stream 'gobject::gobject-class)
    (let ((g-type (gobject::gobject-class-g-type-name widget)))
      (when (g-type-is-a g-type "GtkContainer")
        (unless (string= g-type (gobject::gobject-class-g-type-name (first (class-direct-superclasses widget))))
          (let ((props (gtk::container-class-child-properties g-type)))
            (when props
              (format stream "Child properties:~%")
              (format stream "@itemize~%")
              ;; @item @anchor{signal.$widget.$signal}"$signal". Signature: Type1 Arg1, .., Typen Argn => return-type. Options: $(signal-options)
              (iter (for prop in (sort (copy-list props) #'string< :key #'g-class-property-definition-name))
                    (for accessor = (format nil "~A-child-~A"
                                            (string-downcase (symbol-name (class-name widget)))
                                            (g-class-property-definition-name prop)))
                    (format stream "@item @anchor{childprop.~A.~A}~A. Type: ~A. Accessor: ~A."
                            (string-downcase (symbol-name (class-name widget)))
                            (g-class-property-definition-name prop)
                            (g-class-property-definition-name prop)
                            (type-string (g-class-property-definition-type prop))
                            accessor)
                    (format stream "~%"))
              (format stream "@end itemize~%"))))))))

(defun signal-signature (s)
  (with-output-to-string (stream)
    (format stream "(instance ~A)" (type-string (signal-info-owner-type s)))
    (iter (for type in (signal-info-param-types s))
          (for counter from 1)
          (format stream ", (arg-~A ~A)" counter (type-string type)))
    (format stream " @result{} ~A" (type-string (signal-info-return-type s)))))

(defun signal-options (s)
  (format nil "~{~A~^, ~}"(signal-info-flags s)))

(defun slot-type (slot)
  (let ((type (gobject::gobject-direct-slot-definition-g-property-type slot)))
    (type-string type)))

(defun type-string (type)
  (typecase type
    (string (type-string-s type))
    (t (type-string-f type))))

(defun ensure-list (x) (if (listp x) x (list x)))

(defun type-string-f (type)
  (let ((l (ensure-list type)))
    (case (first l)
      (glib:gstrv "list of @code{string}")
      ((:string glib:g-string) "@code{string}")
      ((:int :uint :long :ulong :char :uchar :int64 :uint64) "@code{integer}")
      ((:boolean :bool) "@code{boolean}")
      (g-object (if (second l)
                    (format-ref (second l))
                    "@ref{g-object}"))
      (g-boxed-foreign (format-ref (second l)))
      ((nil) "????")
      ((glist gslist) (format nil "list of ~A" (type-string-f (second l))))
      (t (if (symbolp type)
             (format-ref type)
             (format-ref l))))))

(defun type-string-s (type)
  (cond
    ((g-type= type "GStrv") "list of @code{string}")
    ((g-type= type +g-type-string+) "@code{string}")
    ((g-type= type +g-type-boolean+) "@code{boolean}")
    ((g-type= type +g-type-float+) "@code{single-float}")
    ((g-type= type +g-type-double+) "@code{double-float}")
    ((or (g-type= type +g-type-int+)
         (g-type= type +g-type-uint+)
         (g-type= type +g-type-char+)
         (g-type= type +g-type-uchar+)
         (g-type= type +g-type-long+)
         (g-type= type +g-type-ulong+)
         (g-type= type +g-type-int64+)
         (g-type= type +g-type-uint64+)
         (g-type= type +g-type-uint64+)) "@code{integer}")
    ((g-type= type +g-type-float+) "@code{single-float}")
    ((g-type-is-a type +g-type-enum+) (enum-string type))
    ((g-type-is-a type +g-type-flags+) (flags-string type))
    ((g-type-is-a type +g-type-object+) (object-string type))
    ((g-type-is-a type +g-type-boxed+) (boxed-string type))
    (t type)))

(defun format-ref (s)
  (if (and *use-refs* (if (symbolp s)
                          (not (eq (symbol-package s) (find-package :cl)))
                          t))
      (format nil "@ref{~A}" s)
      (format nil "@code{~A}" s)))

(defun flags-string (type)
  (let ((flags (gobject::registered-flags-type (g-type-string type))))
    (if flags
        (format-ref flags)
        (format nil "@code{~A}" (g-type-string type)))))

(defun enum-string (type)
  (let ((enum (gobject::registered-enum-type (g-type-string type))))
    (if enum
        (format-ref enum)
        (format nil "@code{~A}" (g-type-string type)))))

(defun object-string (type)
  (let ((class (gobject::registered-object-type-by-name (g-type-string type))))
    (if class
        (format-ref class)
        (format nil "@code{~A}" (g-type-string type)))))

(defun boxed-string (type)
  (let ((boxed (ignore-errors (gobject::get-g-boxed-foreign-info-for-gtype (g-type-string type)))))
    (if boxed
        (format-ref (gobject::g-boxed-info-name boxed))
        (format nil "@code{~A}" (g-type-string type)))))

(defmethod classify-slot-readability (class (slot gobject::gobject-property-direct-slot-definition))
  (let* ((g-type (gobject::gobject-class-g-type-name class))
         (property-name (gobject::gobject-property-direct-slot-definition-g-property-name slot))
         (prop (if (g-type-is-a g-type +g-type-interface+)
                   (find property-name (interface-properties g-type)
                         :test #'string=
                         :key #'g-class-property-definition-name)
                   (class-property-info g-type property-name)))
         (readable (g-class-property-definition-readable prop))
         (writable (g-class-property-definition-writable prop)))
    (cond
      ((and readable writable) :normal)
      ((not readable) :write-only)
      ((not writable) :read-only)
      (t :bad))))

(defmethod classify-slot-readability (class (slot gobject::gobject-fn-direct-slot-definition))
  (let ((readable (gobject::gobject-fn-direct-slot-definition-g-getter-name slot))
        (writable (gobject::gobject-fn-direct-slot-definition-g-setter-name slot)))
    (cond
      ((and readable writable) :normal)
      ((not readable) :write-only)
      ((not writable) :read-only)
      (t :bad))))

(defun slot-accessor (slot)
  (let* ((readers (slot-definition-readers slot))
         (writers (mapcar #'second (slot-definition-writers slot)))
         (combined (union readers writers))
         (accessor (first combined)))
    (if accessor
        (format nil "@anchor{fn.~A}@code{~A}" accessor accessor)
        (format nil "None"))))

;; Enum skeleton
;; (enum-skeleton enum &key (section "section"))
;; @node $enum
;; @section $enum
;; Values:
;; @itemize
;; $(enum-values enum)
;; @end itemize

(defun enum-skeleton (enum &key (section "section"))
  (with-output-to-string (stream)
    (format stream "@node ~A~%" (string-downcase enum))
    (format stream "@~A ~A~%" section (string-downcase enum))
    (format stream "@Enum ~A~%" (string-downcase enum))
    (format stream "Values: ~%")
    (format stream "@itemize~%")
    (iter (for item in (sort (copy-list (cffi:foreign-enum-keyword-list enum)) #'string<))
          (format stream "@item @anchor{enum.~A.~A}:~A~%"
                  (string-downcase enum)
                  (string-downcase (symbol-name item))
                  (string-downcase (symbol-name item))))
    (format stream "@end itemize~%")))

(defun flags-skeleton (flags &key (section "section"))
  (with-output-to-string (stream)
    (format stream "@node ~A~%" (string-downcase flags))
    (format stream "@~A ~A~%" section (string-downcase flags))
    (format stream "@Flags ~A~%" (string-downcase flags))
    (format stream "Values: ~%")
    (format stream "@itemize~%")
    (iter (for item in (sort (copy-list (cffi:foreign-bitfield-symbol-list flags)) #'string<))
          (format stream "@item @anchor{flags.~A.~A}:~A~%"
                  (string-downcase flags)
                  (string-downcase (symbol-name item))
                  (string-downcase (symbol-name item))))
    (format stream "@end itemize~%")))

(defun all-enums (package)
  (sort (iter (for symbol in-package package :external-only t)
              (unless (member symbol *exclusions*)
                (when (ignore-errors (cffi:foreign-enum-keyword-list symbol))
                  (collect symbol))))
        #'string<))

(defun all-flags (package)
  (sort (iter (for symbol in-package package :external-only t)
              (unless (member symbol *exclusions*)
                (when (ignore-errors (cffi:foreign-bitfield-symbol-list symbol))
                  (collect symbol))))
        #'string<))

(defun enum-chapter-skeleton (output enums &key (section "section"))
  (cond
    ((or (pathnamep output) (stringp output))
     (with-open-file (stream output :direction :output :if-exists :supersede)
       (enum-chapter-skeleton stream enums :section section)))
    ((null output) (with-output-to-string (stream)
                     (enum-chapter-skeleton stream enums :section section)))
    ((or (eq t output) (streamp output))
     (format output "@menu~%")
     (iter (for e in enums)
           (format output "* ~A::~%" (string-downcase (symbol-name e))))
     (format output "@end menu~%~%")
     (iter (for e in enums)
           (write-string (enum-skeleton e :section section) output)
           (format output "~%~%")))))

(defun flags-chapter-skeleton (output flagss &key (section "section"))
  (cond
    ((or (pathnamep output) (stringp output))
     (with-open-file (stream output :direction :output :if-exists :supersede)
       (flags-chapter-skeleton stream flagss :section section)))
    ((null output) (with-output-to-string (stream)
                     (flags-chapter-skeleton stream flagss :section section)))
    ((or (eq t output) (streamp output))
     (format output "@menu~%")
     (iter (for e in flagss)
           (format output "* ~A::~%" (string-downcase (symbol-name e))))
     (format output "@end menu~%~%")
     (iter (for e in flagss)
           (write-string (flags-skeleton e :section section) output)
           (format output "~%~%")))))

;; Struct skeleton
;; (struct-skeleton struct &key (section "section") (use-refs t))
;; @node $struct
;; @$section $struct
;; @Struct @struct
;; Slots:
;; @itemize
;; $(for each slot
;; @item $slot
;; )
;; @end itemize

(defun struct-skeleton (struct &key (section "section") (use-refs t))
  (unless (typep struct 'class) (setf struct (find-class struct)))
  (with-output-to-string (stream)
    (let ((*print-case* :downcase)
          (*package* (symbol-package (class-name struct)))
          (*print-circle* nil)
          (*use-refs* use-refs))
      (format stream "@node ~A~%" (class-name struct))
      (format stream "@~A ~A~%" section (class-name struct))
      (format stream "@Struct ~A~%" (class-name struct))
      (format stream "Superclass:")
      (iter (for super in (class-direct-superclasses struct))
            (format stream " ~A" (format-ref (class-name super))))
      (when (class-direct-subclasses struct)
        (format stream "~%~%")
        (format stream "Subclasses:")
        (iter (for sub in (class-direct-subclasses struct))
              (unless (member (class-name sub) *ref-exclusions*)
                (format stream " ~A" (format-ref (class-name sub))))))
      (format stream "~%~%")
      (struct-slots stream struct))))

(defun struct-slots (stream struct)
  (format stream "Slots:~%")
  (format stream "@itemize~%")
  (iter (for slot in (sort (copy-list (class-direct-slots struct)) #'string< :key #'slot-definition-name))
        (format stream "@item @anchor{slot.~A.~A}~A. Accessor: ~A."
                (class-name struct) (string-downcase (slot-definition-name slot))
                (string-downcase (slot-definition-name slot))
                (format nil "~A-~A" (class-name struct) (slot-definition-name slot)))
        (format stream "~%"))
  (format stream "@end itemize~%"))

(defun all-structs (package)
  (sort (iter (for symbol in-package package :external-only t)
              (unless (member symbol *exclusions*)
                (for class = (find-class symbol nil))
                (when (and class (typep class (find-class 'structure-class)))
                  (collect symbol))))
        #'string<))

(defun struct-chapter-skeleton (output structs &key (section "section") (use-refs t))
  (cond
    ((or (stringp output) (pathnamep output))
     (with-open-file (stream output :direction :output :if-exists :supersede)
       (struct-chapter-skeleton stream structs :section section :use-refs use-refs)))
    ((null output) (with-output-to-string (stream)
                     (struct-chapter-skeleton stream structs :section section :use-refs use-refs)))
    ((or (eq t output) (streamp output))
     (format output "@menu~%")
     (iter (for e in structs)
           (format output "* ~A::~%" (string-downcase (symbol-name e))))
     (format output "@end menu~%~%")
     (iter (for e in structs)
           (write-string (struct-skeleton e :section section :use-refs use-refs) output)
           (format output "~%~%")))))

(defun interface-chapter-skeleton (output interfaces &key (use-refs t) (section "section"))
  (cond
    ((or (stringp output) (pathnamep output))
     (with-open-file (stream output :direction :output :if-exists :supersede)
       (interface-chapter-skeleton stream interfaces :use-refs use-refs :section section)))
    ((null output) (with-output-to-string (stream)
                     (interface-chapter-skeleton stream interfaces :use-refs use-refs :section section)))
    ((or (eq t output) (streamp output))
     (format output "@menu~%")
     (iter (for w in interfaces)
           (format output "* ~A::~%" (string-downcase (symbol-name w))))
     (format output "@end menu~%~%")
     (iter (for w in interfaces)
           (write-string (interface-skeleton w :section section :use-refs use-refs) output)
           (format output "~%~%")))))

(defun all-interfaces (package)
  (sort (iter (for symbol in-package package :external-only t)
              (unless (member symbol *exclusions*)
                (for class = (find-class symbol nil))
                (when (and class
                           (typep class 'gobject:gobject-class)
                           (gobject::gobject-class-interface-p class))
                  (collect symbol))))
        #'string<))

;; (interface-skeleton interface &key (sectioning-command "section"))
;; returns the texinfo string for interface (a symbol or class)
;; Template:
;; 
;; @node $INTERFACE
;; @$SECTIONING-COMMAND $INTERFACE
;;
;; @Class $INTERFACE
;; 
;; Interfaces: $(direct-interface interface)
;;
;; Slots:
;; @itemize
;; $(for each slot)
;; @item @anchor{slot.$interface.$slot}$slot. Type: $(slot-type slot). Accessor: $(slot-accessor slot). $(when (constructor-only slot) "Contructor-only slot.")
;; $(end for)
;; @end itemize
;;
;; Signals:
;; @itemize
;; $(for each signal)
;; @item @anchor{signal.$interface.$signal}"$signal". Signature: Type1 Arg1, .., Typen Argn => return-type. Options: $(signal-options)
;; $(end for)
;; @end itemize

(defun interface-skeleton (interface &key (section "section") (use-refs t))
  (unless (typep interface 'class) (setf interface (find-class interface)))
  (with-output-to-string (stream)
    (let ((*print-case* :downcase)
          (*package* (symbol-package (class-name interface)))
          (*print-circle* nil)
          (*use-refs* use-refs))
      (format stream "@node ~A~%" (class-name interface))
      (format stream "@~A ~A~%" section (class-name interface))
      (format stream "@Class ~A~%" (class-name interface))
      (when (class-direct-subclasses interface)
        (format stream "~%~%")
        (format stream "Subclasses:")
        (iter (for sub in (class-direct-subclasses interface))
              (unless (member (class-name sub) *ref-exclusions*)
                (format stream " ~A" (format-ref (class-name sub))))))
      (format stream "~%~%")
      (widget-slots stream interface)
      (format stream "~%~%")
      (widget-signals stream interface))))

(defun all-gtk-skeletons (dir)
  (widgets-chapter-skeleton (merge-pathnames "gdk.objects.texi" dir) (all-classes :gdk))
  (widgets-chapter-skeleton (merge-pathnames "gtk.objects.texi" dir) (all-classes :gtk))
  (struct-chapter-skeleton (merge-pathnames "gtk.structs.texi" dir) (all-structs :gtk))
  (struct-chapter-skeleton (merge-pathnames "gdk.structs.texi" dir) (all-structs :gdk))
  (widgets-chapter-skeleton (merge-pathnames "gtk.widgets.texi" dir) (all-widgets :gtk))
  (interface-chapter-skeleton (merge-pathnames "gtk.interfaces.texi" dir) (all-interfaces :gtk))
  (enum-chapter-skeleton (merge-pathnames "gtk.enums.texi" dir) (all-enums :gtk))
  (enum-chapter-skeleton (merge-pathnames "gdk.enums.texi" dir) (all-enums :gdk))
  (flags-chapter-skeleton (merge-pathnames "gtk.flags.texi" dir) (all-flags :gtk))
  (flags-chapter-skeleton (merge-pathnames "gdk.flags.texi" dir) (all-flags :gdk)))
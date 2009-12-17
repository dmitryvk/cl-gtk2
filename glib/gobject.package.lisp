(defpackage :gobject
  (:use :c2cl :glib :cffi :tg :bordeaux-threads :iter :closer-mop :gobject.ffi)
  (:export #:g-type
           #:g-type-string
           #:g-type-numeric
           #:g-type-children
           #:g-type-parent
           #:g-type-designator
           #:g-type-fundamental
           #:g-type-depth
           #:g-type-next-base
           #:g-type-is-a
           #:g-type-interfaces
           #:g-type-interface-prerequisites
           #:g-strv-get-type
           #:g-closure-get-type
           #:g-class-property-definition
           #:g-class-property-definition-name
           #:g-class-property-definition-type
           #:g-class-property-definition-readable
           #:g-class-property-definition-writable
           #:g-class-property-definition-constructor
           #:g-class-property-definition-constructor-only
           #:g-class-property-definition-owner-type
           #:parse-g-param-spec
           #:class-properties
           #:interface-properties
           #:enum-item
           #:enum-item-name
           #:enum-item-value
           #:enum-item-nick
           #:get-enum-items
           #:flags-item
           #:flags-item-name
           #:flags-item-value
           #:flags-item-nick
           #:get-flags-items
           #:signal-info
           #:signal-info-id
           #:signal-info-name
           #:signal-info-owner-type
           #:signal-info-flags
           #:signal-info-return-type
           #:signal-info-param-types
           #:signal-info-detail
           #:query-signal-info
           #:type-signals
           #:parse-signal-name
           #:class-property-info
           #:+g-type-invalid+
           #:+g-type-void+
           #:+g-type-interface+
           #:+g-type-char+
           #:+g-type-uchar+
           #:+g-type-boolean+
           #:+g-type-int+
           #:+g-type-uint+
           #:+g-type-long+
           #:+g-type-ulong+
           #:+g-type-int64+
           #:+g-type-uint64+
           #:+g-type-enum+
           #:+g-type-flags+
           #:+g-type-float+
           #:+g-type-double+
           #:+g-type-string+
           #:+g-type-pointer+
           #:+g-type-boxed+
           #:+g-type-param+
           #:+g-type-object+
           #:g-object
           #:pointer
           #:g-type-from-object
           #:g-type-name
           #:g-type-from-name
           #:g-signal-connect
           #:define-g-object-class
           #:g-initially-unowned
           #:define-g-enum
           #:*lisp-name-package*
           #:define-g-flags
           #:fixed-array
           #:g-boxed-inline
           #:g-boxed-ptr
           #:define-g-interface
           #:release
           #:using
           #:using*
           #:g-boxed-ref
           #:allocate-stable-pointer
           #:free-stable-pointer
           #:get-stable-pointer-value
           #:with-stable-pointer
           #:release*
           #:disown-boxed-ref
           #:g-type-interface
           #:g-value
           #:register-object-type-implementation
           #:ensure-g-type
           #:define-vtable
           #:g-type
           #:set-g-value
           #:parse-g-value
           #:emit-signal
           #:g-value-unset
           #:g-value-zero
           #:g-value-init
           #:g-type-class-ref
           #:g-object-class
           #:gobject-class
           #:g-param-spec
           #:type-instance
           #:g-type-class-unref
           #:registered-object-type-by-name
           #:g-type-children
           #:g-signal-lookup
           #:g-type-parent
           #:connect-signal
           #:boxed-c-structure-name
           #:g-type-designator
           #:g-type-fundamental
           #:g-type-depth
           #:g-type-next-base
           #:g-type-is-a
           #:g-type-interfaces
           #:g-type-interface-prerequisites
           #:g-type-name
           #:g-type-from-name
           #:g-type
           #:g-type-children
           #:g-type-parent
           #:g-type-designator
           #:g-type-fundamental
           #:g-type-depth
           #:g-type-next-base
           #:g-type-is-a
           #:g-type-interfaces
           #:g-type-interface-prerequisites
           #:g-strv-get-type
           #:g-closure-get-type
           #:g-class-property-definition
           #:g-class-property-definition-name
           #:g-class-property-definition-type
           #:g-class-property-definition-readable
           #:g-class-property-definition-writable
           #:g-class-property-definition-constructor
           #:g-class-property-definition-constructor-only
           #:g-class-property-definition-owner-type
           #:parse-g-param-spec
           #:class-properties
           #:interface-properties
           #:enum-item
           #:enum-item-name
           #:enum-item-value
           #:enum-item-nick
           #:get-enum-items
           #:flags-item
           #:flags-item-name
           #:flags-item-value
           #:flags-item-nick
           #:get-flags-items
           #:stable-pointer-value
           #:g-value-type
           #:g-object-call-constructor
           #:g-object-call-get-property
           #:g-object-call-set-property
           #:register-enum-type
           #:register-flags-type
           #:register-object-type
           #:generate-types-hierarchy-to-file
           #:get-g-flags-definition
           #:get-g-enum-definition
           #:get-g-interface-definition
           #:get-g-class-definition
           #:*strip-prefix*
           #:*lisp-name-exceptions*
           #:*additional-properties*
           #:g-type=
           #:g-type/=
           #:define-g-boxed-cstruct
           #:define-g-boxed-opaque
           #:g-boxed-opaque
           #:g-boxed-opaque-pointer
           #:define-g-boxed-variant-cstruct
           #:g-boxed-foreign
           #:boxed-related-symbols
           #:define-boxed-opaque-accessor
           #:glib-defcallback
           #:create-signal-handler-closure
           #:save-handler-to-object
           #:retrieve-handler-from-object
           #:delete-handler-from-object
           #:disconnect-signal
           #:define-cb-methods
           #:create-fn-ref
           #:copy-boxed-slots-to-foreign
           #:with-foreign-boxed-array
           #:get-g-type-definition)
  (:documentation
   "CL-GTK2-GOBJECT is a binding to GObject type system.
For information on GObject, see its @a[http://library.gnome.org/devel/gobject/stable/]{documentation}.

CL-GTK2-GOBJECT is structured as follows:
@itemize{
@item{Binding to GObject API, providing low-level means to use functionality of GObject. This includes introspection facilities and means to invoke functionality of GObject.}
@item{GObject wrapper that bridges Lisp language with GObject API.}
}

@begin[GObject instrospection API]{section}
The base of GObject type system is GType. GType is a numerical value that is the unique identifier of a registered type.
Each GType has a name that is retrieved with @fun{g-type-name}. Conversely, GType can be retrieved from its name via @fun{g-type-from-name}.

There are several predefined GType values that correspond to fundamental or base types.
@begin{itemize}
@item{@variable{+g-type-invalid+}}
@item{@variable{+g-type-void+}}
@item{@variable{+g-type-interface+}}
@item{@variable{+g-type-char+}}
@item{@variable{+g-type-uchar+}}
@item{@variable{+g-type-boolean+}}
@item{@variable{+g-type-int+}}
@item{@variable{+g-type-uint+}}
@item{@variable{+g-type-long+}}
@item{@variable{+g-type-ulong+}}
@item{@variable{+g-type-int64+}}
@item{@variable{+g-type-uint64+}}
@item{@variable{+g-type-enum+}}
@item{@variable{+g-type-flags+}}
@item{@variable{+g-type-float+}}
@item{@variable{+g-type-double+}}
@item{@variable{+g-type-string+}}
@item{@variable{+g-type-pointer+}}
@item{@variable{+g-type-boxed+}}
@item{@variable{+g-type-param+}}
@item{@variable{+g-type-object+}}
@end{itemize}

GType values form type hierarchies via signle inheritance. Functions @fun{g-type-parent} and @fun{g-type-children} enable to traverse through the type hierarchy.

For some types, additional information is available. Functions @fun{class-properties} and @fun{interface-properties} return properties of classes and interfaces. Functions @fun{get-enum-items} and @fun{get-flags-items} return members of enum and flags types.

//TODO: document and refactor signals

@end{section}

@begin[GValue]{section}
GObject uses GValues as a generic way to pass values. It is used when calling closures, emitting signals, setting and getting properties' values, passing values to object constructors. @class{g-value} foreign structure is used for holding GValue. It used like all foreign structures: either with @code{cffi:foreign-alloc} or with @code{cffi:with-foreign-object}. Before first use, @class{g-value} should be zeroed with @fun{g-value-zero}. Zeroed @class{g-value} may be configured to hold a GValue of a given type with @fun{g-value-init}. @fun{parse-g-value} retrieves the Lisp object corresponding to the value stored in GValue. @fun{set-g-value} sets the GValue from Lisp object.
@end{section}"))

(in-package :gobject)

(defvar *gobject-debug* nil)

(defvar *debug-gc* nil)
(defvar *debug-subclass* nil)

(defvar *debug-stream* t)

(defmacro log-for (categories control-string &rest args)
  (let ((vars (iter (for sym in (if (listp categories) categories (list categories)))
                    (collect (intern (format nil "*DEBUG-~A*" (symbol-name sym)) (find-package :gobject))))))
    `(progn
       (when (or ,@vars)
         (format *debug-stream* ,control-string ,@args))
       nil)))

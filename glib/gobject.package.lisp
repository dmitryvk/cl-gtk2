(defpackage :gobject
  (:use :cl :glib :cffi :tg :bordeaux-threads :iter :closer-mop)
  (:export #:g-object
           #:pointer
           #:g-type-from-object
           #:g-type-name
           #:g-type-from-name
           #:g-signal-connect
           #:define-g-object-class
           #:g-initially-unowned
           #:define-g-enum
           #:*lisp-name-package*
           #:define-g-boxed-class
           #:define-g-flags
           #:fixed-array
           #:g-boxed-inline
           #:g-boxed-ptr
           #:define-g-interface
           #:release
           #:using
           #:using*
           #:define-g-boxed-ref
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
           #:parse-gvalue
           #:emit-signal
           #:g-value-unset
           #:g-value-zero
           #:g-value-init
           #:g-class-property-definition
           #:g-class-property-definition-name
           #:g-class-property-definition-type
           #:g-class-property-definition-readable
           #:g-class-property-definition-writable
           #:g-class-property-definition-constructor
           #:g-class-property-definition-constructor-only
           #:g-class-property-definition-owner-type
           #:g-type-class-ref
           #:g-object-class
           #:gobject-class
           #:g-param-spec
           #:type-instance
           #:parse-g-param-spec
           #:g-type-class-unref
           #:registered-object-type-by-name
           #:g-type-children
           #:g-signal-lookup
           #:g-type-parent
           #:connect-signal
           #:boxed-c-structure-name)
  (:documentation
"This package contains bindings to GLib object system called GObject.

It contains:
@begin{itemize}
@item{type system}
@item{object system}
@item{utilities for memory management}
@end{itemize}

@begin[GObject type system querying]{section}
GObject type information can queried. Type is identified by GType — an integer.

Function @fun{g-type-from-name} returns GType by its name, and function @fun{g-type-name} returns the name of a GType. Function @fun{ensure-g-type} is a convenience function that returns GType from either GType or its name.

Functions @fun{g-type-parent} and @fun{g-type-children} inspect type hierarchy.
@end{section}

@begin[GValue]{section}
GValue is a GObject's \"variant\"-like type. It can contain value of any type that GObject supports. The GValue is used for passing parameters to signal handlers, properties' getters and setters. Functions @fun{g-value-init}, @fun{g-value-unset}, @fun{g-value-zero}, @fun{parse-gvalue} and @fun{set-g-value} are used to init, reset, retrieve and assign GValue structures.
@end{section}

@begin[Utilities]{section}
GObject contains implementation of stable pointers. Stable pointer is a reference that can be passed between FFI boundaries. Stable pointer is an integer that is allocated with @fun{allocate-stable-pointer}. This integer can be passed to other code and back. The value of this pointer (that is retrieved with @fun{get-stable-pointer-value}) is stable, staying the same until the pointer is freed with @fun{free-stable-pointer}. For convenience, @fun{with-stable-pointer} macro is provided.
@end{section}"))

(in-package :gobject)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library gobject
    (:unix (:or "libgobject-2.0.so.0" "libgobject-2.0.so"))
    (t "libgobject-2.0")))

(use-foreign-library gobject)

(defvar *gobject-debug* nil)

(defun debugf (&rest args)
  (when *gobject-debug*
    (apply 'format t args)))
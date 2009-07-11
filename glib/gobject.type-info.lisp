(defpackage :gobject.type-info
  (:use :cl :iter :cffi :glib)
  (:export #:+g-type-invalid+
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
           #:g-closure-get-type)
  (:documentation
"This package contains functions for querying the basic type information from GObject type system. For an overview of GObject type system, see @a[http://library.gnome.org/devel/gobject/stable/index.html]{GObject documentation}

Types are identified by GType designators that are specified in @class{g-type-designator}. Functions of this package provide means to query basic information about type.

@begin{itemize}
@item{@fun{g-type-name} and @fun{g-type-from-name} convert between numeric and string representation of GType.}
@item{@fun{g-type-parent}, @fun{g-type-children} and @fun{g-type-interfaces} traverse across the type hierarchy.}
@item{@fun{g-type-depth}, @fun{g-type-fundamental}, @fun{g-type-is-a}, @fun{g-type-next-base} are convenience functions that provide useful information from type hierarchy}
@end{itemize}

This is a list of variables and functions that correspond to basic types:
@begin{itemize}
@item{@fun{g-closure-get-type}}
@item{@fun{g-strv-get-type}}
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
"))

(in-package :gobject.type-info)

(defctype g-type gsize)

(eval-when (:load-toplevel :compile-toplevel)
  (defun gtype-make-fundamental-type (x)
    (ash x 2)))

(defconstant +g-type-invalid+ (gtype-make-fundamental-type 0) "An invalid GType used as error return value in some functions which return a GType.")
(defconstant +g-type-void+ (gtype-make-fundamental-type 1) "A fundamental type which is used as a replacement for the C @code{void} return type.")
(defconstant +g-type-interface+ (gtype-make-fundamental-type 2) "The fundamental type from which all interfaces are derived.")
(defconstant +g-type-char+ (gtype-make-fundamental-type 3) "The fundamental type corresponding to gchar. The type designated by @variable{+g-type-char+} is unconditionally an 8-bit signed integer. This may or may not be the same type a the C type @code{gchar}.")
(defconstant +g-type-uchar+ (gtype-make-fundamental-type 4) "The fundamental type corresponding to @code{guchar}.")
(defconstant +g-type-boolean+ (gtype-make-fundamental-type 5) "The fundamental type corresponding to @code{gboolean}.")
(defconstant +g-type-int+ (gtype-make-fundamental-type 6) "The fundamental type corresponding to @code{gint}.")
(defconstant +g-type-uint+ (gtype-make-fundamental-type 7) "The fundamental type corresponding to @code{guint}.")
(defconstant +g-type-long+ (gtype-make-fundamental-type 8) "The fundamental type corresponding to @code{glong}.")
(defconstant +g-type-ulong+ (gtype-make-fundamental-type 9) "The fundamental type corresponding to @code{gulong}.")
(defconstant +g-type-int64+ (gtype-make-fundamental-type 10) "The fundamental type corresponding to @code{gint64}.")
(defconstant +g-type-uint64+ (gtype-make-fundamental-type 11) "The fundamental type corresponding to @code{guint64}.")
(defconstant +g-type-enum+ (gtype-make-fundamental-type 12) "The fundamental type from which all enumeration types are derived.")
(defconstant +g-type-flags+ (gtype-make-fundamental-type 13) "The fundamental type from which all flags types are derived.")
(defconstant +g-type-float+ (gtype-make-fundamental-type 14) "The fundamental type corresponding to @code{gfloat}.")
(defconstant +g-type-double+ (gtype-make-fundamental-type 15) "The fundamental type corresponding to @code{gdouble}.")
(defconstant +g-type-string+ (gtype-make-fundamental-type 16) "The fundamental type corresponding to null-terminated C strings.")
(defconstant +g-type-pointer+ (gtype-make-fundamental-type 17) "The fundamental type corresponding to @code{gpointer}.")
(defconstant +g-type-boxed+ (gtype-make-fundamental-type 18) "The fundamental type from which all boxed types are derived.")
(defconstant +g-type-param+ (gtype-make-fundamental-type 19) "The fundamental type from which all GParamSpec types are derived.")
(defconstant +g-type-object+ (gtype-make-fundamental-type 20) "The fundamental type for GObject.")

(define-foreign-type g-type-designator ()
  ()
  (:documentation "Values of this CFFI foreign type identify the GType. GType is designated by a its name (a string) or a numeric identifier. Functions accept GType designators as a string or integer and return them as a string. Functions @fun{g-type-name} and @fun{g-type-from-name} are used to convert between name and numeric identifier.

Numeric identifier of GType may be different between different program runs. But string identifier of GType does not change.")
  (:actual-type g-type)
  (:simple-parser g-type-designator))

(defmethod translate-from-foreign (value (type g-type-designator))
  (g-type-name value))

(defmethod translate-to-foreign (value (type g-type-designator))
  (etypecase value
    (string (g-type-from-name value))
    (integer value)
    (null +g-type-invalid+)))

(defcfun (g-type-fundamental "g_type_fundamental") g-type-designator
  "Returns the fundamental type which is the ancestor of @code{type}.

Example:
@pre{
\(g-type-fundamental \"GtkWindowType\")
=> \"GEnum\"
\(g-type-fundamental \"GtkLabel\")
=> \"GObject\"
}
@arg[type]{GType designator (see @class{g-type-designator})}
@return{GType designator}"
  (type g-type-designator))

(defcfun (%g-type-init "g_type_init") :void)

(at-init () (%g-type-init))

(defcfun (g-type-name "g_type_name") :string
  "Returns the name of a GType.@see{g-type-from-name}

Example:
@pre{
\(g-type-from-name \"GtkLabel\")
=> 7151952
\(g-type-name 7151952)
=> \"GtkLabel\"
}
@arg[type]{GType designator (see @class{g-type-designator})}
@return{a string}"
  (type g-type-designator))

(defcfun (g-type-from-name "g_type_from_name") g-type
  "Returns the numeric identifier of a GType by its name. @see{g-type-name}

Example:
@pre{
\(g-type-from-name \"GtkLabel\")
=> 7151952
\(g-type-name 7151952)
=> \"GtkLabel\"
}
@arg[name]{a string - name of GType}
@return{an integer}"
  (name :string))

(defcfun g-type-parent g-type-designator
  "Returns the parent of a GType. @see{g-type-chilren}

Example:
@pre{
\(g-type-parent \"GtkLabel\")
=> \"GtkMisc\"
}
@arg[type]{GType designator (see @class{g-type-designator})}
@return{GType designator}"
  (type g-type-designator))

(defcfun g-type-depth :uint
  "Returns the length of the ancestry of @code{type}. This includes the @code{type} itself, so that e.g. a fundamental type has depth 1.

Example:
@pre{
\(g-type-depth \"GtkLabel\")
=> 6
}
@arg[type]{GType designator (see @class{g-type-designator})}
@return{an integer}"
  (type g-type-designator))

(defcfun g-type-next-base g-type-designator
  "Determines the type that is derived directly from @code{root-type} which is also a base class of @code{leaf-type}.

Example:
@pre{
\(g-type-next-base \"GtkButton\" \"GtkWidget\")
=> \"GtkContainer\"
}
@arg[leaf-type]{GType designator (see @class{g-type-designator})}
@arg[root-type]{GType designator}
@return{GType designator}"
  (leaf-type g-type-designator)
  (root-type g-type-designator))

(defcfun g-type-is-a :boolean
  "If @code{is-a-type} is a derivable type, check whether type is a descendant of @code{is-a-type}. If @code{is-a-type} is an interface, check whether type conforms to it.

Example:
@pre{
\(g-type-is-a \"GtkButton\" \"GtkWidget\")
=> T
\(g-type-is-a \"GtkButton\" \"AtkImplementorIface\")
=> T
\(g-type-is-a \"GtkButton\" \"GtkLabel\")
=> NIL
}
@arg[type]{GType designator (see @class{g-type-designator})}
@arg[is-a-type]{GType designator}
@return{boolean}"
  (type g-type-designator)
  (is-a-type g-type-designator))

(defcfun (%g-type-children "g_type_children") (:pointer g-type)
  (type g-type-designator)
  (n-children (:pointer :uint)))

(defun g-type-children (g-type)
  "Returns the list of types inherited from @code{g-type}.@see{g-type-parent}

Example:
@pre{
\(g-type-children \"GtkObject\")
=> (\"GtkWidget\" \"GtkAdjustment\" \"GtkTreeViewColumn\" \"GtkCellRenderer\"
    \"GtkFileFilter\" \"GtkRecentFilter\" \"GtkTooltips\")
}
@arg[g-type]{GType designator (see @class{g-type-designator})}
@return{list of GType designators}"
  (with-foreign-object (n-children :uint)
    (let ((g-types-ptr (%g-type-children g-type n-children)))
      (prog1
          (loop
             for i from 0 below (mem-ref n-children :uint)
             collect (mem-aref g-types-ptr 'g-type-designator i))
        (g-free g-types-ptr)))))

(defcfun (%g-type-interfaces "g_type_interfaces") (:pointer g-type)
  (type g-type-designator)
  (n-interfaces (:pointer :uint)))

(defun g-type-interfaces (g-type)
  "Returns the list of interfaces the @code{g-type} conforms to.

Example:
@pre{
\(g-type-interfaces \"GtkButton\")
=> (\"AtkImplementorIface\" \"GtkBuildable\" \"GtkActivatable\")
}
@arg[g-type]{GType designator (see @class{g-type-designator})}
@return{list of GType designators}"
  (with-foreign-object (n-interfaces :uint)
    (let ((g-types-ptr (%g-type-interfaces g-type n-interfaces)))
      (prog1
          (loop
             for i from 0 below (mem-ref n-interfaces :uint)
             collect (mem-aref g-types-ptr 'g-type-designator i))
        (g-free g-types-ptr)))))

(defcfun (%g-type-interface-prerequisites "g_type_interface_prerequisites") (:pointer g-type)
  (type g-type-designator)
  (n-interface-prerequisites (:pointer :uint)))

(defun g-type-interface-prerequisites (g-type)
  "Returns the prerequisites of an interface type. Prerequisite is a type that must be a superclass of an implementing class or an interface that the object must also implement.

Example:
@pre{
\(g-type-interface-prerequisites \"GtkTreeModel\")
=> (\"GObject\")
}
@arg[g-type]{GType designator (see @class{g-type-designator})}
@return{list of GType designators}"
  (with-foreign-object (n-interface-prerequisites :uint)
    (let ((g-types-ptr (%g-type-interface-prerequisites g-type n-interface-prerequisites)))
      (prog1
          (loop
             for i from 0 below (mem-ref n-interface-prerequisites :uint)
             collect (mem-aref g-types-ptr 'g-type-designator i))
        (g-free g-types-ptr)))))

(defcfun g-strv-get-type g-type-designator
  "Returns the type designator (see @class{g-type-designator}) for GStrv type. As a side effect, ensures that the type is registered.")

(at-init nil (g-strv-get-type))

(defcfun g-closure-get-type g-type-designator
  "Returns the type designator (see @class{g-type-designator}) for GClosure type. As a side effect, ensure that the type is registered.")

(at-init nil (g-closure-get-type))

(defpackage :gobject.type-info
  (:use :cl :iter :cffi :glib :gobject.ffi)
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
           #:parse-signal-name)
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


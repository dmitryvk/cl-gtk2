(in-package :gobject)

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


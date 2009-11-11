(defpackage :gtk
  (:use :cl :cffi :gobject :gdk :glib :iter :pango)
  (:export #:gtk-main
           #:gtk-main-quit
           #:dialog-run
           #:object-destroy
           #:text-buffer-insert
           #:define-child-property
           #:container-class-child-properties
           #:generate-child-properties
           #:tree-lisp-store
           #:tree-lisp-store-root
           #:tree-node
           #:make-tree-node
           #:tree-node-tree
           #:tree-node-parent
           #:tree-node-id
           #:tree-node-item
           #:tree-node-children
           #:tree-node-insert-at
           #:tree-node-remove-at
           #:tree-node-child-at
           #:tree-lisp-store-add-column
           #:gtk-main-add-timeout
           #:gtk-call-aborted
           #:gtk-call-aborted-condition
           #:let-ui))

(defpackage :gtk-examples
  (:use :cl :gtk :gdk :gobject)
  (:export #:test-dialog))

(in-package :gtk)

(at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library gtk
      (:unix (:or "libgtk-x11-2.0.so.0" "libgtk-x11-2.0.so"))
      (:windows (:or "libgtk-2.0-0.dll" "libgtk-win32-2.0-0.dll"))
      (t "libgtk-2.0")))

  (use-foreign-library gtk))

#+sbcl (when (and (find-package "SB-EXT")
                  (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")))
         (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")) :traps nil))
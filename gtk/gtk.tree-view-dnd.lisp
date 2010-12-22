(in-package :gtk)

(define-vtable ("GtkTreeDragSource" tree-drag-source)
  (:skip parent-instance g-type-interface)
  ;;methods
  (row-draggable (:boolean
		  (tree-drag-source g-object)
		  (path (g-boxed-foreign tree-path))))
  (drag-data-get (:boolean
		  (tree-drag-source g-object)
		  (path (g-boxed-foreign tree-path))
		  (selection-data (g-boxed-foreign selection-data))))
  (drag-data-delete (:boolean
		     (tree-drag-source g-object)
		     (path (g-boxed-foreign tree-path)))))

(define-vtable ("GtkTreeDragDest" tree-drag-dest)
  (:skip parent-instance g-type-interface)
  ;;methods
  (drag-data-received (:boolean
		       (tree-drag-dest g-object)
		       (path (g-boxed-foreign tree-path))
		       (selection-data (g-boxed-foreign selection-data))))
  (row-drop-possible (:boolean
		      (tree-drag-dest g-object)
		      (path (g-boxed-foreign tree-path))
		      (selection-data (g-boxed-foreign selection-data)))))


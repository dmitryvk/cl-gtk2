(in-package :gtk)

(defcfun (fixed-put "gtk_fixed_put") :void
  (fixed g-object)
  (widget g-object)
  (x :int)
  (y :int))

(export 'fixed-put)

(defcfun (fixed-move "gtk_fixed_move") :void
  (fixed g-object)
  (widget g-object)
  (x :int)
  (y :int))

(export 'fixed-move)

(defcfun (layout-put "gtk_layout_put") :void
  (layout g-object)
  (widget g-object)
  (x :int)
  (y :int))

(export 'layout-put)

(defcfun (layout-move "gtk_layout_move") :void
  (layout g-object)
  (widget g-object)
  (x :int)
  (y :int))

(export 'layout-move)

(defcfun gtk-notebook-append-page :int
  (notebook g-object)
  (child g-object)
  (tab-label g-object))

(defcfun gtk-notebook-append-page-menu :int
  (notebook g-object)
  (child g-object)
  (tab-label g-object)
  (menu g-object))

(defcfun gtk-notebook-prepend-page :int
  (notebook g-object)
  (child g-object)
  (tab-label g-object))

(defcfun gtk-notebook-prepend-page-menu :int
  (notebook g-object)
  (child g-object)
  (tab-label g-object)
  (menu g-object))

(defcfun gtk-notebook-insert-page :int
  (notebook g-object)
  (child g-object)
  (tab-label g-object)
  (position :int))

(defcfun gtk-notebook-insert-page-menu :int
  (notebook g-object)
  (child g-object)
  (tab-label g-object)
  (menu g-object)
  (position :int))

(defun notebook-add-page (notebook child tab-label &key (position :end) menu)
  (assert (typep position '(or integer (member :start :end))))
  (assert (typep menu '(or null g-object (member :default))))
  (case position
    (:end (if menu
              (gtk-notebook-append-page-menu notebook child tab-label (if (eq menu :default) (null-pointer) menu))
              (gtk-notebook-append-page notebook child tab-label)))
    (:start (if menu
                (gtk-notebook-prepend-page-menu notebook child tab-label (if (eq menu :default) (null-pointer) menu))
                (gtk-notebook-prepend-page notebook child tab-label)))
    (otherwise (if menu
                (gtk-notebook-insert-page-menu notebook child tab-label (if (eq menu :default) (null-pointer) menu) position)
                (gtk-notebook-insert-page notebook child tab-label position)))))

(export 'notebook-add-page)

(defcfun (notebook-page-num "gtk_notebook_page_num") :int
  (notebook g-object)
  (child g-object))

(export 'notebook-page-num)

(defcfun gtk-notebook-remove-page :void
  (notebook g-object)
  (page-num :int))

(defun notebook-remove-page (notebook page-or-number)
  (gtk-notebook-remove-page notebook (etypecase page-or-number
                                       (integer page-or-number)
                                       (widget (notebook-page-num notebook page-or-number)))))

(export 'notebook-remove-page)

(defcfun (notebook-next-page "gtk_notebook_next_page") :void
  (notebook g-object))

(export 'notebook-next-page)

(defcfun (notebook-prev-page "gtk_notebook_prev_page") :void
  (notebook g-object))

(export 'notebook-prev-page)

(defcfun (notebook-reorder-child "gtk_notebook_reorder_child") :void
  (notebook g-object)
  (child g-object)
  (position :int))

(export 'notebook-reorder-child)

(defcfun (notebook-menu-label-widget "gtk_notebook_get_menu_label") g-object
  (notebook g-object)
  (child g-object))

(export 'notebook-menu-label-widget)

(defcfun (notebook-nth-page "gtk_notebook_get_nth_page") g-object
  (notebook g-object)
  (page-num :int))

(export 'notebook-nth-page)

(defcfun (notebook-n-pages "gtk_notebook_get_n_pages") :int
  (notebook g-object))

(export 'notebook-n-pages)

(defcfun (notebook-tab-label-widget "gtk_notebook_get_tab_label") g-object
  (notebook g-object)
  (child g-object))

(export 'notebook-tab-label-widget)

(defcfun (gtk-notebook-set-menu-label-widget "gtk_notebook_set_menu_label") :void
  (notebook g-object)
  (child g-object)
  (menu-label g-object))

(defun (setf notebook-menu-label-widget) (new-value notebook child)
  (gtk-notebook-set-menu-label-widget notebook child new-value)
  new-value)

(defcfun (gtk-notebook-set-tab-label-widget "gtk_notebook_set_tab_label") :void
  (notebook g-object)
  (child g-object)
  (tab-label g-object))

(defun (setf notebook-tab-label-widget) (new-value notebook child)
  (gtk-notebook-set-tab-label-widget notebook child new-value)
  new-value)

(defcallback gtk-notebook-window-creation-func-callback g-object
    ((source g-object) (page g-object) (x :int) (y :int) (data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value data)
               source page x y)
    (return-null () nil)))

(defcfun gtk-notebook-set-window-creation-hook :void
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun notebook-set-window-creation-hook (function)
  (gtk-notebook-set-window-creation-hook (callback gtk-notebook-window-creation-func-callback)
                                         (allocate-stable-pointer function)
                                         (callback stable-pointer-free-destroy-notify-callback)))

(export 'notebook-set-window-creation-hook)

(defcfun gtk-table-attach :void
  (table (g-object table))
  (child (g-object widget))
  (left-attach :uint)
  (right-attach :uint)
  (top-attach :uint)
  (bottom-attach :uint)
  (x-options attach-options)
  (y-options attach-options)
  (x-padding :uint)
  (y-padding :uint))

(defun table-attach (table widget left right top bottom &key (x-options '(:expand :fill)) (y-options '(:expand :fill)) (x-padding 0) (y-padding 0))
  (gtk-table-attach table widget left right top bottom x-options y-options x-padding y-padding))

(export 'table-attach)

(defcfun (table-row-spacing-for-row "gtk_table_get_row_spacing") :uint
  (table g-object)
  (row :uint))

(defcfun gtk-table-set-row-spacing :void
  (table g-object)
  (row :uint)
  (spacing :uint))

(defun (setf table-row-spacing-for-row) (new-value table row)
  (gtk-table-set-row-spacing table row new-value))

(export 'table-row-spacing-for-row)

(defcfun (table-col-spacing-for-col "gtk_table_get_col_spacing") :uint
  (table g-object)
  (col :uint))

(defcfun gtk-table-set-col-spacing :void
  (table g-object)
  (col :uint)
  (spacing :uint))

(defun (setf table-col-spacing-for-col) (new-value table col)
  (gtk-table-set-col-spacing table col new-value))

(export 'table-col-spacing-for-col)
(in-package :gtk)

(defcfun (menu-reorder-child "gtk_menu_reorder_child") :void
  (menu g-object)
  (child g-object)
  (position :int))

(export 'menu-reorder-child)

(defcfun (menu-attach "gtk_menu_attach") :void
  (menu g-object)
  (child g-object)
  (left-attach :uint)
  (right-attach :uint)
  (top-attach :uint)
  (bottom-attach :uint))

(export 'menu-attach)

(defcfun gtk-menu-popup :void
  (menu g-object)
  (parent-menu-shell g-object)
  (parent-menu-item g-object)
  (menu-position-func :pointer)
  (data :pointer)
  (button :uint)
  (activate-time :uint32))

(defcallback gtk-menu-position-func-callback :void
    ((menu g-object) (x :pointer) (y :pointer) (push-in :pointer) (data :pointer))
  (restart-case
      (multiple-value-bind (rx ry rpush-in) (funcall (get-stable-pointer-value data)
                                                     menu)
        (setf (mem-ref x :int) rx
              (mem-ref y :int) ry
              (mem-ref push-in :boolean) rpush-in))
    (return-zero () (setf (mem-ref x :int) 0
                          (mem-ref y :int) 0
                          (mem-ref push-in :boolean) nil))))

(defun menu-popup (menu &key parent-menu-shell parent-menu-item position-func (button 0) (activate-time (current-event-time)))
  (if position-func
      (with-stable-pointer (ptr position-func)
        (gtk-menu-popup menu parent-menu-shell parent-menu-item
                        (callback gtk-menu-position-func-callback)
                        ptr button activate-time))
      (gtk-menu-popup menu parent-menu-shell parent-menu-item (callback gtk-menu-position-func-callback) (null-pointer) (null-pointer) activate-time)))

(export 'menu-popup)

(defcfun (menu-popdown "gtk_menu_popdown") :void
  (menu g-object))

(export 'menu-popdown)

(defcfun (menu-reposition "gtk_menu_reposition") :void
  (menu g-object))

(export 'menu-reposition)

; TODO: gtk_menu_attach_to_widget

(defcfun (menu-detach "gtk_menu_detach") :void
  (menu (g-object menu)))

(export 'menu-detach)

; TODO: gtk_menu_get_attach_to_widget

(defcfun (menu-attached-to-widget "gtk_menu_get_for_attach_widget") (glist (g-object menu) :free-from-foreign nil)
  (width (g-object widget)))

(export 'menu-attached-to-widget)

(defcfun (tool-shell-rebuild-menu "gtk_tool_shell_rebuild_menu") :void
  (shell (g-object tool-shell)))

(export 'tool-shell-rebuild-menu)

(defcfun (menu-shell-append "gtk_menu_shell_append") :void
  (menu-shell g-object)
  (child g-object))

(export 'menu-shell-append)

(defcfun (menu-shell-prepend "gtk_menu_shell_prepend") :void
  (menu-shell g-object)
  (child g-object))

(export 'menu-shell-prepend)

(defcfun (menu-shell-insert "gtk_menu_shell_insert") :void
  (menu-shell g-object)
  (child g-object)
  (position :int))

(export 'menu-shell-insert)

(defcfun (menu-shell-deactivate "gtk_menu_shell_deactivate") :void
  (menu-shell g-object))

(export 'menu-shell-deactivate)

(defcfun (menu-shell-select-item "gtk_menu_shell_select_item") :void
  (menu-shell g-object)
  (menu-item g-object))

(export 'menu-shell-select-item)

(defcfun gtk-menu-shell-select-first :void
  (menu-shell g-object)
  (search-sensitive :boolean))

(defun menu-shell-select-first (menu-shell &optional (search-sensitive t))
  (gtk-menu-shell-select-first menu-shell search-sensitive))

(export 'menu-shell-select-first)

(defcfun (menu-shell-deselect "gtk_menu_shell_deselect") :void
  (menu-shell g-object))

(export 'menu-shell-deselect)

(defcfun gtk-menu-shell-activate-item :void
  (menu-shell g-object)
  (menu-item g-object)
  (force-deactivate :boolean))

(defun menu-shell-activate-item (menu-shell menu-item &optional force-deactivate)
  (gtk-menu-shell-activate-item menu-shell menu-item force-deactivate))

(export 'menu-shell-activate-item)

(defcfun (menu-shell-cancel "gtk_menu_shell_cancel") :void
  (menu-shell g-object))

(export 'menu-shell-cancel)

; TODO: GtkToolShell

(defcfun (toolbar-insert "gtk_toolbar_insert") :void
  (toolbar g-object)
  (item g-object)
  (pos :int))

(export 'toolbar-insert)

(defcfun (toolbar-item-index "gtk_toolbar_get_item_index") :int
  (toolbar g-object)
  (item g-object))

(export 'toolbar-item-index)

(defcfun (toolbar-items-count "gtk_toolbar_get_n_items") :int
  (toolbar g-object))

(export 'toolbar-items-count)

(defcfun (toolbar-nth-item "gtk_toolbar_get_nth_item") g-object
  (toolbar g-object)
  (n :int))

(export 'toolbar-nth-item)

(defcfun (toolbar-get-drop-index "gtk_toolbar_get_drop_index") :int
  (toolbar g-object)
  (x :int)
  (y :int))

(export 'toolbar-get-drop-index)

(defcfun (toolbar-set-drop-highlight-item "gtk_toolbar_set_drop_highlight_item") :void
  (toolbar g-object)
  (tool-item g-object)
  (index :int))

(export 'toolbar-set-drop-highlight-item)

(defcfun (toolbar-unset-style "gtk_toolbar_unset_style") :void
  (toolbar g-object))

(export 'toolbar-unset-style)

(defcfun (tool-item-retrieve-proxy-menu-item "gtk_tool_item_retrieve_proxy_menu_item") g-object
  (tool-item g-object))

(export 'tool-item-retrieve-proxy-menu-item)

; TODO: gtk_tool_item_get_proxy_menu_item

; TODO: gtk_tool_item_set_proxy_menu_item

(defcfun (tool-item-rebuild-menu "gtk_tool_item_rebuild_menu") :void
  (tool-item g-object))

(export 'tool-item-rebuild-menu)

(defcfun (radio-tool-button-get-group "gtk_radio_tool_button_get_group")
    (gslist (g-object radio-tool-button) :free-from-foreign nil)
  (button (g-object radio-tool-button)))

(in-package :gtk)

(defcfun (ui-manager-insert-action-group "gtk_ui_manager_insert_action_group") :void
  (ui-manager g-object)
  (action-group g-object)
  (pos :int))

(export 'ui-manager-insert-action-group)

(defcfun (ui-manager-remove-action-group "gtk_ui_manager_remove_action_group") :void
  (ui-manager g-object)
  (action-group g-object))

(export 'ui-manager-remove-action-group)

(defcfun (ui-manager-action-groups "gtk_ui_manager_get_action_groups") (glist g-object :free-from-foreign nil)
  (ui-manager g-object))

(export 'ui-manager-action-groups)

(defcfun (ui-manager-widget "gtk_ui_manager_get_widget") g-object
  (ui-manager g-object)
  (path :string))

(export 'ui-manager-widget)

(defcfun (ui-manager-toplevels "gtk_ui_manager_get_toplevels") (gslist g-object :free-from-foreign t)
  (ui-manager g-object)
  (types ui-manager-item-type))

(export 'ui-manager-toplevels)

(defcfun (ui-manager-action "gtk_ui_manager_get_action") g-object
  (ui-manager g-object)
  (path :string))

(export 'ui-manager-action)

(defcfun gtk-ui-manager-add-ui-from-string :uint
  (ui-manager g-object)
  (buffer :string)
  (length gssize)
  (error :pointer))

(defun ui-manager-add-ui-from-string (ui-manager string)
  (with-g-error (err)
    (gtk-ui-manager-add-ui-from-string ui-manager string -1 err)))

(export 'ui-manager-add-ui-from-string)

(defcfun gtk-ui-manager-add-ui-from-file :uint
  (ui-manager g-object)
  (file-name :string)
  (error :pointer))

(defun ui-manager-add-ui-from-file (ui-manager file-name)
  (with-g-error (err)
    (gtk-ui-manager-add-ui-from-file ui-manager file-name err)))

(export 'ui-manager-add-ui-from-file)

(defcfun (ui-manager-new-merge-id "gtk_ui_manager_new_merge_id") :uint
  (ui-manager g-object))

(export 'ui-manager-new-merge-id)

(defcfun (ui-manager-add-ui "gtk_ui_manager_add_ui") :void
  (ui-manager g-object)
  (merge-id :uint)
  (path :string)
  (name :string)
  (action :string)
  (type ui-manager-item-type)
  (top :boolean))

(export 'ui-manager-add-ui)

(defcfun (ui-manager-remove-ui "gtk_ui_manager_remove_ui") :void
  (ui-manager g-object)
  (merge-id :uint))

(export 'ui-manager-remove-ui)

(defcfun (ui-manager-ensure-update "gtk_ui_manager_ensure_update") :void
  (ui-manager g-object))

(export 'ui-manager-remove-ui)

(defcfun (action-group-action "gtk_action_group_get_action") g-object
  (action-group g-object)
  (action-name :string))

(export 'action-group-action)

(defcfun (action-group-actions "gtk_action_group_list_actions") (glist g-object :free-from-foreign t)
  (action-group g-object))

(export 'action-group-actions)

(defcfun gtk-action-group-add-action-with-accel :void
  (action-group g-object)
  (action g-object)
  (accelerator :string))

(defun action-group-add-action (action-group action &key accelerator)
  (gtk-action-group-add-action-with-accel action-group action (if accelerator accelerator (null-pointer))))

(export 'action-group-add-action)

(defcfun (action-group-remove-action "gtk_action_group_remove_action") :void
  (action-group g-object)
  (action g-object))

(export 'action-group-remove-action)

(defcallback gtk-translate-func-callback (:string :free-to-foreign nil :free-from-foreign nil)
    ((path (:string :free-from-foreign nil)) (data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value data)
               path)
    (return-untranslated () path)))

(defcfun gtk-action-group-set-translate-func :void
  (action-group g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun action-group-set-translate-func (action-group func)
  (gtk-action-group-set-translate-func action-group
                                       (callback gtk-translate-func-callback)
                                       (allocate-stable-pointer func)
                                       (callback stable-pointer-free-destroy-notify-callback)))

(defcfun gtk-action-group-set-translation-domain :void
  (action-group g-object)
  (domain :string))

(defcfun (action-group-translate-string "gtk_action_group_translate_string") (:string :free-from-foreign nil)
  (action-group g-object)
  (string (:string :free-to-foreign nil)))

(export 'action-group-translate-string)

(defcfun (action-is-sensitive "gtk_action_is_sensitive") :boolean
  (action g-object))

(export 'action-is-sensitive)

(defcfun (action-is-visible "gtk_action_is_visible") :boolean
  (action g-object))

(export 'action-is-visible)

(defcfun (action-create-icon "gtk_action_create_icon") g-object
  (action g-object)
  (icon-size icon-size))

(export 'action-create-icon)

(defcfun (action-create-menu-item "gtk_action_create_menu_item") g-object
  (action g-object))

(export 'action-create-menu-item)

(defcfun (action-create-tool-item "gtk_action_create_tool_item") g-object
  (action g-object))

(export 'action-create-tool-item)

(defcfun (action-create-menu "gtk_action_create_menu") g-object
  (action g-object))

(export 'action-create-menu)

(defcfun (action-connect-proxy "gtk_action_connect_proxy") :void
  (action g-object)
  (proxy g-object))

(export 'action-connect-proxy)

(defcfun (action-disconnect-proxy "gtk_action_disconnect_proxy") :void
  (action g-object)
  (proxy g-object))

(export 'action-disconnect-proxy)

(defcfun (action-proxies "gtk_action_get_proxies") (gslist g-object :free-from-foreign nil)
  (action g-object))

(export 'action-proxies)

(defcfun (action-connect-accelerator "gtk_action_connect_accelerator") :void
  (action g-object))

(export 'action-connect-accelerator)

(defcfun (action-disconnect-accelerator "gtk_action_disconnect_accelerator") :void
  (action g-object))

(export 'action-disconnect-accelerator)

(defcfun (action-block-activate "gtk_action_block_activate") :void
  (action (g-object action)))

(export 'action-block-activate)

(defcfun (action-unblock-activate "gtk_action_unblock_activate") :void
  (action (g-object action)))

(export 'action-unblock-activate)

(defcfun (action-block-activate-from "gtk_action_block_activate_from") :void
  (action g-object)
  (proxy g-object))

(export 'action-block-activate-from)

(defcfun (action-unblock-activate-from "gtk_action_unblock_activate_from") :void
  (action g-object)
  (proxy g-object))

(export 'action-unblock-activate-from)

(defcfun (radio-action-get-group "gtk_radio_action_get_group") (gslist (g-object radio-action) :free-from-foreign nil)
  (action (g-object radio-action)))

(export 'radio-action-get-group)

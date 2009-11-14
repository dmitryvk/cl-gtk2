(in-package :gdk)

(defcstruct %gdk-drag-context
  (parent-instance gobject.ffi::%g-object)
  (protocol gdk-drag-protocol)
  (is-source :boolean)
  (source-window (g-object gdk-window))
  (dest-window (g-object gdk-window))
  (targets (glib:glist gdk-atom-as-string :free-from-foreign nil))
  (actions gdk-drag-action)
  (suggested-action gdk-drag-action)
  (action gdk-drag-action)
  (start-time :uint32))

(defun %gdk-drag-context-get-protocol (context)
  (foreign-slot-value (pointer context) '%gdk-drag-context 'protocol))

(defun %gdk-drag-context-get-is-source (context)
  (foreign-slot-value (pointer context) '%gdk-drag-context 'is-source))

(defun %gdk-drag-context-get-source-window (context)
  (foreign-slot-value (pointer context) '%gdk-drag-context 'source-window))

(defun %gdk-drag-context-get-dest-window (context)
  (foreign-slot-value (pointer context) '%gdk-drag-context 'dest-window))

(defun %gdk-drag-context-get-targets (context)
  (foreign-slot-value (pointer context) '%gdk-drag-context 'targets))

(defun %gdk-drag-context-get-actions (context)
  (foreign-slot-value (pointer context) '%gdk-drag-context 'actions))

(defun %gdk-drag-context-get-suggested-action (context)
  (foreign-slot-value (pointer context) '%gdk-drag-context 'suggested-action))

(defun %gdk-drag-context-get-action (context)
  (foreign-slot-value (pointer context) '%gdk-drag-context 'action))

(defun %gdk-drag-context-get-start-time (context)
  (foreign-slot-value (pointer context) '%gdk-drag-context 'start-time))

(defcfun gdk-drag-get-selection gdk-atom-as-string
  (context (g-object drag-context)))

(export 'gdk-drag-get-selection)

(defcfun gdk-drag-abort :void
  (context (g-object drag-context))
  (time :uint32))

(export 'gdk-drag-abord)

(defcfun gdk-drop-reply :void
  (context (g-object drag-context))
  (ok :boolean)
  (time :uint32))

(export 'gdk-drop-reply)

(defcfun gdk-drag-drop :void
  (context (g-object drag-context))
  (time :uint32))

(export 'gdk-drag-drop)

(defcfun gdk_drag_find_window :void
  (context (g-object drag-context))
  (window (g-object gdk-window))
  (x-root :int)
  (y-root :int)
  (dest-window (:pointer (g-object gdk-window)))
  (protocol (:pointer gdk-drag-protocol)))

(defun gdk-drag-find-window (context window x-root y-root)
  (with-foreign-objects ((dest-window :pointer) (protocol 'gdk-drag-protocol))
    (gdk_drag_find_window context window x-root y-root dest-window protocol)
    (values (mem-ref dest-window '(g-object gdk-window))
            (mem-ref protocol 'gdk-drag-protocol))))

(export 'gdk-drag-find-window)

(defcfun gdk_drag_find_window_for_screen :void
  (context (g-object drag-context))
  (window (g-object gdk-window))
  (screen (g-object screen))
  (x-root :int)
  (y-root :int)
  (dest-window (:pointer (g-object gdk-window)))
  (protocol (:pointer gdk-drag-protocol)))

(defun gdk-drag-find-window-for-screen (context window screen x-root y-root)
  (with-foreign-objects ((dest-window :pointer) (protocol 'gdk-drag-protocol))
    (gdk_drag_find_window_for_screen context window screen x-root y-root dest-window protocol)
    (values (mem-ref dest-window '(g-object gdk-window))
            (mem-ref protocol 'gdk-drag-protocol))))

(export 'gdk-drag-find-window-for-screen)

(defcfun gdk-drag-begin (g-object gdk-drag-context :already-referenced)
  (window (g-object gdk-window))
  (targets (glib:glist gdk-atom-as-string)))

(export 'gdk-drag-begin)

(defcfun gdk-drag-motion :boolean
  (context (g-object drag-context))
  (dest-window (g-object gdk-window))
  (protocol gdk-drag-protocol)
  (x-root :int)
  (y-root :int)
  (suggested-action gdk-drag-action)
  (possible-actions gdk-drag-action)
  (time :uint32))

(export 'gdk-drag-motion)

(defcfun gdk-drop-finish :void
  (context (g-object drag-context))
  (success :boolean)
  (time :uint32))

(export 'gdk-drop-finish)

(defcfun gdk_drag_get_protocol native-window
  (xid native-window)
  (protocol (:pointer gdk-drag-protocol)))

(defun gdk-drag-get-protocol (xid)
  (with-foreign-object (protocol 'gdk-drag-protocol)
    (let ((dest-window (gdk_drag_get_protocol xid protocol)))
      (values dest-window (mem-ref protocol 'gdk-drag-protocol)))))

(export 'gdk-drag-get-protocol)

(defcfun gdk_drag_get_protocol_for_display native-window
  (display (g-object display))
  (xid native-window)
  (protocol (:pointer gdk-drag-protocol)))

(defun gdk-drag-get-protocol-for-dispaly (display xid)
  (with-foreign-object (protocol 'gdk-drag-protocol)
    (let ((dest-window (gdk_drag_get_protocol_for_display display xid protocol)))
      (values dest-window (mem-ref protocol 'gdk-drag-protocol)))))

(export 'gdk-drag-get-protocol-for-display)

(defcfun gdk-drag-status :void
  (context (g-object drag-context))
  (action gdk-drag-action)
  (time :uint32))

(export 'gdk-drag-status)

(defcfun gdk-drag-drop-succeeded :boolean
  (context (g-object drag-context)))

(export 'gdk-drag-drop-succeeded)

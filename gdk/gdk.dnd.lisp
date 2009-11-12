(in-package :gdk)

(gobject:define-g-enum "GdkDragProtocol" drag-protocol
  (:export t :type-initializer "gdk_drag_protocol_get_type")
  (:motif 0)
  (:xdnd 1)
  (:rootwin 2)
  (:none 3)
  (:win32-dropfiles 4)
  (:ole2 5)
  (:local 6))

(define-g-flags "GdkDragAction" drag-action
    (:export t :type-initializer "gdk_drag_action_get_type")
  (:default 1)
  (:copy 2)
  (:move 4)
  (:link 8)
  (:private 16)
  (:ask 32))

(defcfun gdk-drag-get-selection gdk-atom
  (context g-object))

(defun drag-get-selection (context)
  (gdk-drag-get-selection context))

(export 'drag-get-selection)

(defcfun gdk-drag-abort :void
  (context g-object)
  (time :uint32))

(defun drag-abort (context time)
  (gdk-drag-abort context time))

(export 'drag-abort)

(defcfun gdk-drop-reply :void
  (context g-object)
  (ok :boolean)
  (time :uint32))

(defun drop-reply (context ok time)
  (gdk-drop-reply context ok time))

(export 'drop-reply)

;; TODO : GdkDragContext * gdk_drag_context_new(void);

(defcfun gdk-drag-drop :void
  (context g-object)
  (time :uint32))

(defun drag-drop (context time)
  (gdk-drag-drop context time))

(export 'drag-drop)

#|
void                gdk_drag_find_window                (GdkDragContext *context,
                                                         GdkWindow *drag_window,
                                                         gint x_root,
                                                         gint y_root,
                                                         GdkWindow **dest_window,
                                                         GdkDragProtocol *protocol);

void                gdk_drag_find_window_for_screen     (GdkDragContext *context,
                                                         GdkWindow *drag_window,
                                                         GdkScreen *screen,
                                                         gint x_root,
                                                         gint y_root,
                                                         GdkWindow **dest_window,
                                                         GdkDragProtocol *protocol);

GdkDragContext *    gdk_drag_begin                      (GdkWindow *window,
                                                         GList *targets);

gboolean            gdk_drag_motion                     (GdkDragContext *context,
                                                         GdkWindow *dest_window,
                                                         GdkDragProtocol protocol,
                                                         gint x_root,
                                                         gint y_root,
                                                         GdkDragAction suggested_action,
                                                         GdkDragAction possible_actions,
                                                         guint32 time_);
|#

(defcfun gdk-drop-finish :void
  (context g-object)
  (success :boolean)
  (time :uint32))

(defun drop-finish (context success time)
  (gdk-drop-finish context success time))

(export 'drop-finish)

(defcfun gdk-drag-get-protocol native-window
  (xid native-window)
  (protocol :pointer))

(defun drag-get-protocol (xid)
  (with-foreign-objects ((protocol 'drag-protocol))
    (let ((result (gdk-drag-get-protocol xid protocol)))
      (values result
              (mem-ref protocol 'drag-protocol)))))

(export 'drag-get-protocol)

(defcfun gdk-drag-get-protocol-for-display native-window
  (display g-object)
  (xid native-window)
  (protocol :pointer))
  
(defun drag-get-protocol-for-display (display xid)
  (with-foreign-objects ((protocol 'drag-protocol))
    (let ((result (gdk-drag-get-protocol-for-display display xid protocol)))
      (values result
              (mem-ref protocol 'drag-protocol)))))

(export 'drag-get-protocol-for-display)

(defcfun gdk-drag-status :void
  (context g-object)
  (action drag-action)
  (time :uint))

(defun drag-status (context action time)
  (gdk-drag-status context action time))

(export 'drag-status)

(defcfun gdk-drag-drop-succeeded :boolean
  (context g-object))

(defun drag-drop-succeeded (context)
  (gdk-drag-drop-succeeded context))

(export 'drag-drop-succeeded)


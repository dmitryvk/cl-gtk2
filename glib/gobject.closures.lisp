(in-package :gobject)

(defcfun g-closure-ref (:pointer g-closure)
  (closure (:pointer g-closure)))

(defcfun g-closure-sink :void
  (closure (:pointer g-closure)))

(defcfun g-closure-unref :void
  (closure (:pointer g-closure)))

(defcfun g-closure-invalidate :void
  (closure (:pointer g-closure)))

(defcfun g-closure-add-finalize-notifier :void
  (closure (:pointer g-closure))
  (notify-data :pointer)
  (notify-func :pointer))

(defcfun g-closure-add-invalidate-notifier :void
  (closure (:pointer g-closure))
  (notify-data :pointer)
  (notify-func :pointer))

(defcfun g-closure-new-simple (:pointer g-closure)
  (sizeof-closure :uint)
  (data :pointer))

(defcfun g-closure-set-marshal :void
  (closure (:pointer g-closure))
  (marshal :pointer))
(in-package :gtk)

(defcfun gtk-image-get-animation (g-object pixbuf-animation)
  (image (g-object image)))

(defcfun gtk-image-set-from-animation :void
  (image (g-object image))
  (animation (g-object pixbuf-animation)))

(defun image-animation (image)
  (gtk-image-get-animation image))

(defun (setf image-animation) (animation image)
  (gtk-image-set-from-animation image animation))

(export 'image-animation)
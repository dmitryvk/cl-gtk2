(in-package :gdk)

(defparameter *selection-primary* "PRIMARY")
(export '*selection-primary*)
(defparameter *selection-secondary* "SECONDARY")
(export '*selection-secondary*)
(defparameter *selection-clipboard* "CLIPBOARD")
(export '*selection-clipboard*)
(defparameter *target-bitmap* "BITMAP")
(export '*target-bitmap*)
(defparameter *target-colormap* "COLORMAP")
(export '*target-colormap*)
(defparameter *target-drawable* "DRAWABLE")
(export '*target-drawable*)
(defparameter *target-pixmap* "PIXMAP")
(export '*target-pixmap*)
(defparameter *target-string* "STRING")
(export '*target-string*)
(defparameter *selection-type-atom* "ATOM")
(export '*selection-type-atom*)
(defparameter *selection-type-bitmap* "BITMAP")
(export '*selection-type-bitmap*)
(defparameter *selection-type-colormap* "COLORMAP")
(export '*selection-type-colormap*)
(defparameter *selection-type-drawable* "DRAWABLE")
(export '*selection-type-drawable*)
(defparameter *selection-type-integer* "INTEGER")
(export '*selection-type-integer*)
(defparameter *selection-type-pixmap* "PIXMAP")
(export '*selection-type-pixmap*)
(defparameter *selection-type-window* "WINDOW")
(export '*selection-type-window*)
(defparameter *selection-type-string* "STRING")
(export '*selection-type-string*)

(defcfun gdk-selection-owner-set :boolean
  (owner (g-object gdk-window))
  (selection gdk-atom-as-string)
  (time :uint32)
  (send-event :boolean))

(export 'gdk-selection-owner-set)

(defcfun gdk-selection-owner-set-for-display :boolean
  (display (g-object display))
  (owner (g-object gdk-window))
  (selection gdk-atom-as-string)
  (time :uint32)
  (send-event :boolean))

(export 'gdk-selection-owner-set-for-display)

(defcfun gdk-selection-owner-get (g-object gdk-window)
  (selection gdk-atom-as-string))

(export 'gdk-selection-owner-get)

(defcfun gdk-selection-owner-get-for-display (g-object gdk-window)
  (display (g-object display))
  (selection gdk-atom-as-string))

(export 'gdk-selection-owner-get-for-display)

(defcfun gdk-selection-convert :void
  (requestor (g-object gdk-window))
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-convert)

(defcfun gdk-selection-property-get :int
  (requestor (g-object gdk-window))
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-property-get)

(defcfun gdk-selection-send-notify :void
  (requestor native-window)
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (property gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-send-notify)

(defcfun gdk-selection-send-notify-for-display :void
  (display (g-object display))
  (requestor native-window)
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (property gdk-atom-as-string)
  (time :uint32))

(export 'gdk-selection-send-notify-for-display)

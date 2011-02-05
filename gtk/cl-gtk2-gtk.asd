(defpackage #:cl-gtk2-gtk-system
  (:use #:cl #:asdf))

(in-package #:cl-gtk2-gtk-system)

(defclass plain-file (static-file)
  ((type :initarg :type :reader plain-file-type :initform nil)))

(defmethod source-file-type ((c plain-file) (s module))
  (plain-file-type c))

(defsystem :cl-gtk2-gtk
  :name :cl-gtk2-gtk
  :version "0.1.1"
  :author "Kalyanov Dmitry <Kalyanov.Dmitry@gmail.com>"
  :license "LLGPL"
  :serial t
  :components ((:file "gtk.package")
               (:file "gtk.misc")
               (:file "gtk.main_loop_events")
               (:file "gtk.object")
               (:file "gtk.objects")
               (:file "gtk.generated-classes")
               (:file "gtk.functions")
               (:file "gtk.base-classes")
               (:file "gtk.dialog")
               (:file "gtk.window")
               (:file "gtk.window-group")
               (:file "gtk.icon-factory")
               (:file "gtk.image")
               (:file "gtk.label")
               (:file "gtk.progress-bar")
               (:file "gtk.status-bar")
               (:file "gtk.status-icon")
               (:file "gtk.scale-button")
               (:file "gtk.entry")
               (:file "gtk.spin-button")
               (:file "gtk.selections")
               (:file "gtk.dnd")
               (:file "gtk.text")
               (:file "gtk.tree-model")
               (:file "gtk.tree-view-column")
               (:file "gtk.tree-selection")
               (:file "gtk.tree-view")
               (:file "gtk.icon-view")
               (:file "gtk.cell-layout")
               (:file "gtk.cell-renderer")
               (:file "gtk.combo-box")
               (:file "gtk.menu")
               (:file "gtk.ui-manager")
               (:file "gtk.selectors")
               (:file "gtk.layout-containers")
               (:file "gtk.scrolling")
               (:file "gtk.calendar")
               (:file "gtk.size-group")
               (:file "gtk.tooltip")
               (:file "gtk.box")
               (:file "gtk.container")
               (:file "gtk.paned")
               (:file "gtk.child-properties")
               (:file "gtk.widget")
               (:file "gtk.builder")
               (:file "gtk.assistant")
               (:file "gtk.link-button")
               (:file "gtk.list-store")
               (:file "gtk.tree-store")
               (:file "gtk.tree-model-filter")
               (:file "gtk.clipboard")
               (:file "gtk.info-bar")
	       (:file "gtk.spinner")
	       (:file "gtk.markup")

               (:file "gtk.main-loop-events")


               (:file "gtk.generated-child-properties")

               (:file "gtk.high-level")

               (:file "ui-markup")

               (:file "gtk.dialog.example")

               (:file "gtk.demo")
               (:file "gtk.timer")
               (:file "gtk.finalize-classes")
               (:module "demo-files"
                        :pathname "demo/"
                        :components ((:plain-file "demo1" :type "ui")
                                     (:plain-file "text-editor" :type "ui"))))
  :depends-on (:cl-gtk2-glib :cffi :cl-gtk2-gdk :bordeaux-threads :iterate :metabang-bind :alexandria :cl-gtk2-pango))

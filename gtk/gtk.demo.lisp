(defpackage :gtk-demo
  (:use :cl :gtk :gdk :gobject :iter)
  (:export #:demo-all
           #:test
           #:test-entry
           #:table-packing
           #:test-pixbuf
           #:test-image
           #:test-progress-bar
           #:test-statusbar
           #:test-scale-button
           #:test-text-view
           #:demo-code-editor
           #:test-treeview-list
           #:test-combo-box
           #:test-ui-manager
           #:test-color-button
           #:test-color-selection
           #:test-file-chooser
           #:test-font-chooser
           #:test-notebook
           #:test-calendar
           #:test-box-child-property
           #:test-builder
           #:demo-text-editor
           #:demo-class-browser
           #:demo-treeview-tree
           #:test-custom-window
           #:test-assistant
           #:test-entry-completion
           #:test-ui-markup
           #:test-list-store
           #:test-tree-store
           #:test-gdk))

(in-package :gtk-demo)

(defparameter *src-location* (asdf:component-pathname (asdf:find-system :cl-gtk2-gtk)))

(defun test ()
  "A simple test of 'on-expose' event"
  (within-main-loop
    (let ((window (make-instance 'gtk-window :type :toplevel :app-paintable t))
          x y)
      (g-signal-connect window "destroy" (lambda (widget)
                                           (declare (ignore widget))
                                           (leave-gtk-main)))
      (g-signal-connect window "motion-notify-event" (lambda (widget event)
                                                       (declare (ignore widget))
                                                       (setf x (event-motion-x event)
                                                             y (event-motion-y event))
                                                       (widget-queue-draw window)))
      (g-signal-connect window "expose-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (let* ((gdk-window (widget-window window))
                                 (gc (graphics-context-new gdk-window))
                                 (layout (widget-create-pango-layout window (format nil "X: ~F~%Y: ~F" x y))))
                            (draw-layout gdk-window gc 0 0 layout)
                            (setf (graphics-context-rgb-fg-color gc) (make-color :red 65535 :green 0 :blue 0))
                            (multiple-value-bind (x y) (drawable-get-size gdk-window)
                              (draw-line gdk-window gc 0 0 x y)))))
      (g-signal-connect window "configure-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (widget-queue-draw window)))
      (widget-show window)
      (push :pointer-motion-mask (gdk-window-events (widget-window window))))))
  
(defun test-entry ()
  "Testing GtkTextEntry"
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :type :toplevel :title "Testing entry" :border-width 10))
           (box (make-instance 'v-box))
           (entry (make-instance 'entry))
           (button (make-instance 'button :label "OK"))
           (text-buffer (make-instance 'text-buffer))
           (text-view (make-instance 'text-view :buffer text-buffer))
           (button-select (make-instance 'button :label "Select"))
           (button-insert (make-instance 'button :label "Insert")))
      (box-pack-start box (make-instance 'label :label "Enter <b>anything</b> you wish:" :use-markup t) :expand nil)
      (box-pack-start box entry :expand nil)
      (box-pack-start box button :expand nil)
      (box-pack-start box button-select :expand nil)
      (box-pack-start box button-insert :expand nil)
      (let* ((w (make-instance 'scrolled-window)))
        (box-pack-start box w)
        (container-add w text-view))
      (container-add window box)
      (g-signal-connect window "destroy" (lambda (widget) (declare (ignore widget)) (leave-gtk-main)))
      (g-signal-connect window "delete-event" (lambda (widget event)
                                                (declare (ignore widget event))
                                                (let ((dlg (make-instance 'message-dialog
                                                                          :text "Are you sure?"
                                                                          :buttons :yes-no)))
                                                  (let ((response (dialog-run dlg)))
                                                    (object-destroy dlg)
                                                    (not (eq :yes response))))))
      (g-signal-connect button "clicked" (lambda (button)
                                           (declare (ignore button))
                                           (setf (text-buffer-text text-buffer)
                                                 (format nil "~A~%~A" (text-buffer-text text-buffer) (entry-text entry))
                                                 (entry-text entry) "")))
      (g-signal-connect button-select "clicked" (lambda (button)
                                                  (declare (ignore button))
                                                  (editable-select-region entry 5 10)))
      (g-signal-connect button-insert "clicked" (lambda (button)
                                                  (declare (ignore button))
                                                  (editable-insert-text entry "hello" 2)))
      (widget-show window))))

(defun table-packing ()
  "Simple test of packing widgets into GtkTable"
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :type :toplevel :title "Table packing" :border-width 20))
           (table (make-instance 'table :n-rows 2 :n-columns 2 :homogeneous t))
           (button-1 (make-instance 'button :label "Button 1"))
           (button-2 (make-instance 'button :label "Button 2"))
           (button-q (make-instance 'button :label "Quit")))
      (container-add window table)
      (table-attach table button-1 0 1 0 1)
      (table-attach table button-2 1 2 0 1)
      (table-attach table button-q 0 2 1 2)
      (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (g-signal-connect button-q "clicked" (lambda (b) (declare (ignore b)) (object-destroy window)))
      (widget-show window))))

(defun test-pixbuf ()
  "(not completed)"
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :title "Test pixbuf" :width-request 600 :height-request 240))
          (vbox (make-instance 'v-box))
          (eventbox (make-instance 'event-box))
          (vbox-1 (make-instance 'v-box)))
     (container-add window vbox)
     (box-pack-start vbox (make-instance 'label :text "Placing bg image" :font "Times New Roman Italic 10" :color "#00f" :height-request 40))
     (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
     (box-pack-start vbox eventbox)
     (container-add eventbox vbox-1)
     (box-pack-start vbox-1 (make-instance 'label :text "This is the eventbox"))
     (box-pack-start vbox-1 (make-instance 'label :text "The green ball is the bg"))
     (widget-show window))))

(defun test-image ()
  "Using GtkImage with stock icon"
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :title "Test images"))
           (image (make-instance 'image :icon-name "applications-development" :icon-size 6)))
      (container-add window image)
      (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (widget-show window))))

(defun test-progress-bar ()
  "Testing progress-bar"
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :title "Test progress bar"))
           (v-box (make-instance 'v-box))
           (p-bar (make-instance 'progress-bar :test "A process"))
           (button-pulse (make-instance 'button :label "Pulse"))
           (button-set (make-instance 'button :label "Set"))
           (entry (make-instance 'entry)))
      (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (container-add window v-box)
      (box-pack-start v-box p-bar)
      (box-pack-start v-box button-pulse)
      (box-pack-start v-box button-set)
      (box-pack-start v-box entry)
      (g-signal-connect button-pulse "clicked" (lambda (w) (declare (ignore w)) (progress-bar-pulse p-bar)))
      (g-signal-connect button-set "clicked" (lambda (w)
                                               (declare (ignore w))
                                               (setf (progress-bar-fraction p-bar)
                                                     (coerce (read-from-string (entry-text entry)) 'real))))
      (widget-show window))))

(defun test-statusbar ()
  "Test of GtkStatusbar"
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :title "Text status bar"))
           (v-box (make-instance 'v-box))
           (h-box (make-instance 'h-box))
           (label (make-instance 'label :label "Test of status bar" :xalign 0.5 :yalign 0.5))
           (statusbar (make-instance 'statusbar :has-resize-grip t))
           (button-push (make-instance 'button :label "Push"))
           (button-pop (make-instance 'button :label "Pop"))
           (entry (make-instance 'entry))
           (icon (make-instance 'status-icon :icon-name "applications-development")))
      (set-status-icon-tooltip icon "An icon from lisp program")
      (g-signal-connect window "destroy" (lambda (w)
                                           (declare (ignore w))
                                           #+ (or) (setf (status-icon-visible icon) nil)
                                           (leave-gtk-main)))
      (g-signal-connect button-push "clicked" (lambda (b)
                                                (declare (ignore b))
                                                (statusbar-push statusbar "lisp-prog" (entry-text entry))))
      (g-signal-connect button-pop "clicked" (lambda (b)
                                               (declare (ignore b))
                                               (statusbar-pop statusbar "lisp-prog")))
      (g-signal-connect icon "activate" (lambda (i)
                                          (declare (ignore i))
                                          (let ((message-dialog (make-instance 'message-dialog
                                                                               :buttons :ok
                                                                               :text "You clicked on icon!")))
                                            (dialog-run message-dialog)
                                            (object-destroy message-dialog))))
      (container-add window v-box)
      (box-pack-start v-box h-box :expand nil)
      (box-pack-start h-box entry)
      (box-pack-start h-box button-push :expand nil)
      (box-pack-start h-box button-pop :expand nil)
      (box-pack-start v-box label)
      (box-pack-start v-box statusbar :expand nil)
      (widget-show window)
      (setf (status-icon-screen icon) (gtk-window-screen window)))))

(defun test-scale-button ()
  "Test of scale button with icons"
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :type :toplevel :title "Testing scale button"))
           (button (make-instance 'scale-button :icons (list "media-seek-backward" "media-seek-forward" "media-playback-stop" "media-playback-start") :adjustment (make-instance 'adjustment :lower -40 :upper 50 :value 20))))
      (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (container-add window button)
      (widget-show window))))

(defun test-text-view ()
  "Test of GtkTextView"
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :type :toplevel :title "Testing text view" :width-request 400 :height-request 300))
           (button (make-instance 'button :label "Do"))
           (button-insert (make-instance 'button :label "Insert a button!"))
           (bold-btn (make-instance 'button :label "Bold"))
           (buffer (make-instance 'text-buffer :text "Some text buffer with some text inside"))
           (v (make-instance 'text-view :buffer buffer :wrap-mode :word))
           (box (make-instance 'v-box))
           (scrolled (make-instance 'scrolled-window :hscrollbar-policy :automatic :vscrollbar-policy :automatic)))
      (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (g-signal-connect button "clicked" (lambda (b)
                                           (declare (ignore b))
                                           (multiple-value-bind (i1 i2) (text-buffer-get-selection-bounds buffer)
                                             (when (and i1 i2)
                                               (let* ((i1 i1) (i2 i2)
                                                      (dialog (make-instance 'message-dialog :buttons :ok)))
                                                 (setf (message-dialog-text dialog)
                                                       (format nil "selection: from (~A,~A) to (~A,~A)"
                                                               (text-iter-line i1) (text-iter-line-offset i1)
                                                               (text-iter-line i2) (text-iter-line-offset i2)))
                                                 (dialog-run dialog)
                                                 (object-destroy dialog))))))
      (g-signal-connect bold-btn "clicked" (Lambda (b)
                                             (declare (ignore b))
                                             (multiple-value-bind (start end) (text-buffer-get-selection-bounds buffer)
                                               (when (and start end)
                                                 (let* ((start start)
                                                        (end end)
                                                        (tag (text-tag-table-lookup (text-buffer-tag-table buffer) "bold")))
                                                   (if (text-iter-has-tag start tag)
                                                       (text-buffer-remove-tag buffer tag start end)
                                                       (text-buffer-apply-tag buffer tag start end)))))))
      (g-signal-connect button-insert "clicked" (lambda (b)
                                                  (declare (ignore b))
                                                  (let* ((iter (text-buffer-get-iter-at-mark buffer (text-buffer-get-mark buffer "insert")))
                                                         (anchor (text-buffer-insert-child-anchor buffer iter))
                                                         (button (make-instance 'button :label "A button!")))
                                                    (widget-show button)
                                                    (text-view-add-child-at-anchor v button anchor))))
      (let ((tag (make-instance 'text-tag :name "bold" :weight 700)))
        (text-tag-table-add (text-buffer-tag-table buffer) tag)
        (g-signal-connect tag "event"
                          (lambda (tag object event iter)
                            (declare (ignore tag object iter))
                            (when (eq (event-type event) :button-release)
                              (let ((dlg (make-instance 'message-dialog :text "You clicked on bold text." :buttons :ok)))
                                (dialog-run dlg)
                                (object-destroy dlg))))))
      (container-add window box)
      (container-add scrolled v)
      (box-pack-start box button :expand nil)
      (box-pack-start box button-insert :expand nil)
      (box-pack-start box bold-btn :expand nil)
      (box-pack-start box scrolled)
      (widget-show window))))

(defun demo-code-editor ()
  "(unfinished)"
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :type :toplevel :title "Code editor" :width-request 400 :height-request 400 :window-position :center))
           (scrolled (make-instance 'scrolled-window :hscrollbar-policy :automatic :vscrollbar-policy :automatic))
           (buffer (make-instance 'text-buffer))
           (view (make-instance 'text-view :buffer buffer)))
      (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (container-add window scrolled)
      (container-add scrolled view)
      (widget-show window)
      (g-signal-connect buffer "insert-text" (lambda (buffer location text len)
                                               (let* ((buffer buffer)
                                                      (location location))
                                                 (format t "~A~%" (list buffer location text len))))))))

(defstruct tvi title value)

(defun test-treeview-list ()
  "Test of treeview with CL-GTK2-GTK:ARRAY-LIST-STORE"
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :type :toplevel :title "Treeview (list)"))
           (model (make-instance 'array-list-store))
           (scroll (make-instance 'scrolled-window :hscrollbar-policy :automatic :vscrollbar-policy :automatic))
           (tv (make-instance 'tree-view :headers-visible t :width-request 100 :height-request 400 :rules-hint t))
           (h-box (make-instance 'h-box))
           (v-box (make-instance 'v-box))
           (title-entry (make-instance 'entry))
           (value-entry (make-instance 'entry))
           (button (make-instance 'button :label "Add")))
      (store-add-column model "gchararray" #'tvi-title)
      (store-add-column model "gint" #'tvi-value)
      (store-add-item model (make-tvi :title "Monday" :value 1))
      (store-add-item model (make-tvi :title "Tuesday" :value 2))
      (store-add-item model (make-tvi :title "Wednesday" :value 3))
      (store-add-item model (make-tvi :title "Thursday" :value 4))
      (store-add-item model (make-tvi :title "Friday" :value 5))
      (store-add-item model (make-tvi :title "Saturday" :value 6))
      (store-add-item model (make-tvi :title "Sunday" :value 7))
      (setf (tree-view-model tv) model (tree-view-tooltip-column tv) 0)
      (gobject:g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (gobject:g-signal-connect button "clicked" (lambda (b)
                                                   (declare (ignore b))
                                                   (store-add-item model (make-tvi :title (entry-text title-entry)
                                                                                   :value (or (parse-integer (entry-text value-entry) 
                                                                                                             :junk-allowed t)
                                                                                              0)))))
      (g-signal-connect tv "row-activated" (lambda (tv path column)
                                             (declare (ignore tv column))
                                             (format t "You clicked on row ~A~%" (tree-path-indices path))))
      (container-add window v-box)
      (box-pack-start v-box h-box :expand nil)
      (box-pack-start h-box title-entry :expand nil)
      (box-pack-start h-box value-entry :expand nil)
      (box-pack-start h-box button :expand nil)
      (box-pack-start v-box scroll)
      (container-add scroll tv)
      (let ((column (make-instance 'tree-view-column :title "Title" :sort-column-id 0))
            (renderer (make-instance 'cell-renderer-text :text "A text")))
        (tree-view-column-pack-start column renderer)
        (tree-view-column-add-attribute column renderer "text" 0)
        (tree-view-append-column tv column)
        (print (tree-view-column-tree-view column))
        (print (tree-view-column-cell-renderers column)))
      (let ((column (make-instance 'tree-view-column :title "Value"))
            (renderer (make-instance 'cell-renderer-text :text "A text")))
        (tree-view-column-pack-start column renderer)
        (tree-view-column-add-attribute column renderer "text" 1)
        (tree-view-append-column tv column)
        (print (tree-view-column-tree-view column))
        (print (tree-view-column-cell-renderers column)))
      (widget-show window))))

(defun test-combo-box ()
  "Testing GtkComboBox"
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :type :toplevel :title "Treeview (list)"))
           (model (make-instance 'array-list-store))
           (combo-box (make-instance 'combo-box :model model))
           (h-box (make-instance 'h-box))
           (v-box (make-instance 'v-box))
           (title-entry (make-instance 'entry))
           (value-entry (make-instance 'entry))
           (button (make-instance 'button :label "Add")))
      (store-add-column model "gchararray" #'tvi-title)
      (store-add-column model "gint" #'tvi-value)
      (store-add-item model (make-tvi :title "Monday" :value 1))
      (store-add-item model (make-tvi :title "Tuesday" :value 2))
      (store-add-item model (make-tvi :title "Wednesday" :value 3))
      (store-add-item model (make-tvi :title "Thursday" :value 4))
      (store-add-item model (make-tvi :title "Friday" :value 5))
      (store-add-item model (make-tvi :title "Saturday" :value 6))
      (store-add-item model (make-tvi :title "Sunday" :value 7))
      (gobject:g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (gobject:g-signal-connect button "clicked" (lambda (b)
                                                   (declare (ignore b))
                                                   (store-add-item model (make-tvi :title (entry-text title-entry)
                                                                                   :value (or (parse-integer (entry-text value-entry) 
                                                                                                             :junk-allowed t)
                                                                                              0)))))
      (g-signal-connect combo-box "changed" (lambda (c)
                                              (declare (ignore c))
                                              (format t "You clicked on row ~A~%" (combo-box-active combo-box))))
      (container-add window v-box)
      (box-pack-start v-box h-box :expand nil)
      (box-pack-start h-box title-entry :expand nil)
      (box-pack-start h-box value-entry :expand nil)
      (box-pack-start h-box button :expand nil)
      (box-pack-start v-box combo-box)
      (let ((renderer (make-instance 'cell-renderer-text :text "A text")))
        (cell-layout-pack-start combo-box renderer :expand t)
        (cell-layout-add-attribute combo-box renderer "text" 0))
      (let ((renderer (make-instance 'cell-renderer-text :text "A number")))
        (cell-layout-pack-start combo-box renderer :expand nil)
        (cell-layout-add-attribute combo-box renderer "text" 1))
      (widget-show window))))

(defun test-ui-manager ()
  "Testing GtkUIManager"
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :type :toplevel :title "UI Manager" :default-width 200 :default-height 100 :window-position :center))
           (ui-manager (make-instance 'ui-manager))
           (print-confirmation t))
      (ui-manager-add-ui-from-string ui-manager
                                     "
<ui>
  <toolbar action='toolbar1'>
      <separator/>
      <toolitem name='Left' action='justify-left'/>
      <toolitem name='Center' action='justify-center'/>
      <toolitem name='Right' action='justify-right'/>
      <toolitem name='Zoom in' action='zoom-in' />
      <toolitem name='print-confirm' action='print-confirm' />
      <separator/>
  </toolbar>
</ui>")
      (gobject:g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (iter (with fn = (lambda (action) (when print-confirmation (format t "Action ~A with name ~A activated~%" action (action-name action)))))
            (with action-group = (make-instance 'action-group :name "Actions"))
            (finally (let ((a (make-instance 'toggle-action :name "print-confirm" :label "Print" :stock-id "gtk-print-report" :active t)))
                       (g-signal-connect a "toggled" (lambda (action) (setf print-confirmation (toggle-action-active action))))
                       (action-group-add-action action-group a))
                     (ui-manager-insert-action-group ui-manager action-group 0))
            (for (name stock-id) in '(("justify-left" "gtk-justify-left")
                                      ("justify-center" "gtk-justify-center")
                                      ("justify-right" "gtk-justify-right")
                                      ("zoom-in" "gtk-zoom-in")))
            (for action = (make-instance 'action :name name :stock-id stock-id))
            (g-signal-connect action "activate" fn)
            (action-group-add-action action-group action))
      (let ((widget (ui-manager-widget ui-manager "/toolbar1")))
        (when widget
          (container-add window widget)))
      (widget-show window))))

(defun test-color-button ()
  "Test of GtkColorButton"
  (within-main-loop
    (let ((window (make-instance 'gtk-window :title "Color button" :type :toplevel :window-position :center :width-request 100 :height-request 100))
          (button (make-instance 'color-button :title "Color button")))
      (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (g-signal-connect button "color-set" (lambda (b)
                                             (declare (ignore b))
                                             (format t "Chose color ~A~%" (color-button-color button))))
      (container-add window button)
      (widget-show window))))

(defun test-color-selection ()
  "Test of GtkColorSelection"
  (within-main-loop
    (let ((window (make-instance 'gtk-window :title "Color selection" :type :toplevel :window-position :center))
          (selection (make-instance 'color-selection :has-opacity-control t)))
      (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (g-signal-connect selection "color-changed" (lambda (s) (declare (ignore s)) (unless (color-selection-adjusting-p selection) (format t "color: ~A~%" (color-selection-current-color selection)))))
      (container-add window selection)
      (widget-show window))))

(defun test-file-chooser ()
  "Test of GtkFileChooser"
  (within-main-loop
    (let ((window (make-instance 'gtk-window :title "file chooser" :type :toplevel :window-position :center :default-width 100 :default-height 100))
          (v-box (make-instance 'v-box))
          (button (make-instance 'file-chooser-button :action :open))
          (b (make-instance 'button :label "Choose for save" :stock-id "gtk-save")))
      (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (g-signal-connect button "file-set" (lambda (b) (declare (ignore b)) (format t "File set: ~A~%" (file-chooser-filename button))))
      (g-signal-connect b "clicked" (lambda (b)
                                      (declare (ignore b))
                                      (let ((d (make-instance 'file-chooser-dialog :action :save :title "Choose file to save")))
                                        (dialog-add-button d "gtk-save" :accept)
                                        (dialog-add-button d "gtk-cancel" :cancel)
                                        (when (eq (dialog-run d) :accept)
                                          (format t "saved to file ~A~%" (file-chooser-filename d)))
                                        (object-destroy d))))
      (container-add window v-box)
      (box-pack-start v-box button)
      (box-pack-start v-box b)
      (widget-show window))))

(defun test-font-chooser ()
  "GtkFontChooser"
  (within-main-loop
    (let ((window (make-instance 'gtk-window :title "fonts" :type :toplevel :window-position :center :default-width 100 :default-height 100))
          (v-box (make-instance 'v-box))
          (button (make-instance 'font-button :title "Choose font" :font-name "Sans 10")))
      (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (g-signal-connect button "font-set" (lambda (b) (declare (ignore b)) (format t "Chose font ~A~%" (font-button-font-name button))))
      (container-add window v-box)
      (box-pack-start v-box button)
      (widget-show window))))

(defun test-notebook ()
  "Test GtkNotebook"
  (within-main-loop
    (let ((window (make-instance 'gtk-window :title "Notebook" :type :toplevel :window-position :center :default-width 100 :default-height 100))
          (expander (make-instance 'expander :expanded t :label "notebook"))
          (notebook (make-instance 'notebook :enable-popup t)))
      (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (iter (for i from 0 to 5)
            (for page = (make-instance 'label :label (format nil "Label for page ~A" i)))
            (for tab-label = (make-instance 'label :label (format nil "Tab ~A" i)))
            (for tab-button = (make-instance 'button
                                             :image (make-instance 'image :stock "gtk-close" :icon-size 1)
                                             :relief :none))
            (g-signal-connect tab-button "clicked"
                              (let ((page page))
                                (lambda (button)
                                  (declare (ignore button))
                                  (format t "Removing page ~A~%" page)
                                  (notebook-remove-page notebook page))))
            (for tab-hbox = (make-instance 'h-box))
            (box-pack-start tab-hbox tab-label)
            (box-pack-start tab-hbox tab-button)
            (widget-show tab-hbox)
            (notebook-add-page notebook page tab-hbox))
      (container-add window expander)
      (container-add expander notebook)
      (widget-show window))))

(defun calendar-detail (calendar year month day)
  (declare (ignore calendar year month))
  (when (= day 23)
    "!"))

(defun test-calendar ()
  "Test of GtkCalendar"
  (within-main-loop
    (let ((window (make-instance 'gtk-window :title "Calendar" :type :toplevel :window-position :center :default-width 100 :default-height 100))
          (calendar (make-instance 'calendar :detail-function #'calendar-detail)))
      (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (g-signal-connect calendar "day-selected" (lambda (c) (declare (ignore c)) (format t "selected: year ~A month ~A day ~A~%"
                                                                                         (calendar-year calendar)
                                                                                         (calendar-month calendar)
                                                                                         (calendar-day calendar))))
      (container-add window calendar)
      (widget-show window))))

(defun test-box-child-property ()
  "Test of child-property usage"
  (within-main-loop
    (let ((window (make-instance 'gtk-window :title "Text box child property" :type :toplevel :window-position :center :width-request 200 :height-request 200))
          (box (make-instance 'h-box))
          (button (make-instance 'toggle-button :active t :label "Expand")))
      (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (g-signal-connect button "toggled" (lambda (b) (declare (ignore b)) (setf (box-child-expand box button) (toggle-button-active button))))
      (container-add window box)
      (box-pack-start box button)
      (widget-show window))))

(defun test-builder ()
  "Test of GtkBuilder"
  (within-main-loop
    (let ((builder (make-instance 'builder)))
      (builder-add-from-file builder (namestring (merge-pathnames "demo/demo1.ui" *src-location*)))
      (let ((text-view (builder-get-object builder "textview1"))
            (c 0))
        (builder-connect-signals-simple builder `(("toolbutton1_clicked_cb" ,(lambda (b)
                                                                                     (declare (ignore b))
                                                                                     #+nil(print (current-event))
                                                                                     (setf (text-buffer-text (text-view-buffer text-view))
                                                                                           (format nil "Clicked ~A times~%" (incf c)))
                                                                                     (statusbar-pop (builder-get-object builder "statusbar1")
                                                                                                     "times")
                                                                                     (statusbar-push (builder-get-object builder "statusbar1")
                                                                                                      "times"
                                                                                                      (format nil "~A times" c))))
                                                  ("quit_cb" ,(lambda (&rest args)
                                                                      (print args)
                                                                      (object-destroy (builder-get-object builder "window1"))))
                                                  ("about_cb" ,(lambda (&rest args)
                                                                       (print args)
                                                                       (let ((d (make-instance 'about-dialog
                                                                                               :program-name "GtkBuilder text"
                                                                                               :version "0.00001"
                                                                                               :authors '("Dmitry Kalyanov")
                                                                                               :logo-icon-name "gtk-apply")))
                                                                         (dialog-run d)
                                                                         (object-destroy d)))))))
      (g-signal-connect (builder-get-object builder "window1") "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
      (statusbar-push (builder-get-object builder "statusbar1") "times" "0 times")
      (widget-show (builder-get-object builder "window1")))))

(defun read-text-file (file-name)
  (with-output-to-string (str)
    (with-open-file (file file-name)
      (loop
         for line = (read-line file nil nil)
         while line
         do (fresh-line str)
         do (write-string line str)))))

(defun demo-text-editor ()
  "More advanced example: text editor with ability to evaluate lisp expressions"
  (within-main-loop
    (let* ((builder (let ((builder (make-instance 'builder)))
                      (builder-add-from-file builder (namestring (merge-pathnames "demo/text-editor.ui" *src-location*)))
                      builder))
           (window (builder-get-object builder "window1"))
           (text-view (builder-get-object builder "textview1"))
           (statusbar (builder-get-object builder "statusbar1"))
           (file-name nil)
           (modified-p t))
      (statusbar-push statusbar "filename" "Untitled *")
      (labels ((set-properties ()
                 (statusbar-pop statusbar "filename")
                 (statusbar-push statusbar "filename" (format nil "~A~:[~; *~]" (or file-name "Untitled") modified-p)))
               (new (&rest args) (declare (ignore args))
                    (setf file-name nil
                          modified-p t
                          (text-buffer-text (text-view-buffer text-view)) "")
                    (set-properties))
               (cb-open (&rest args) (declare (ignore args))
                        (let ((d (make-instance 'file-chooser-dialog :action :open :title "Open file")))
                          (when file-name (setf (file-chooser-filename d) file-name))
                          (dialog-add-button d "gtk-open" :accept)
                          (dialog-add-button d "gtk-cancel" :cancel)
                          (when (eq :accept (dialog-run d))
                            (setf file-name (file-chooser-filename d)
                                  (text-buffer-text (text-view-buffer text-view)) (read-text-file file-name)
                                  modified-p nil)
                            (set-properties))
                          (object-destroy d)))
               (save (&rest args) (declare (ignore args))
                     (if file-name
                         (progn
                           (with-open-file (file file-name :direction :output :if-exists :supersede)
                             (write-string (text-buffer-text (text-view-buffer text-view)) file))
                           (setf modified-p nil)
                           (set-properties))
                         (save-as)))
               (save-as (&rest args) (declare (ignore args))
                        (let ((d (make-instance 'file-chooser-dialog :action :save :title "Save file")))
                          (when file-name (setf (file-chooser-filename d) file-name))
                          (dialog-add-button d "gtk-save" :accept)
                          (dialog-add-button d "gtk-cancel" :cancel)
                          (if (eq :accept (dialog-run d))
                              (progn
                                (setf file-name (file-chooser-filename d))
                                (object-destroy d)
                                (save))
                              (object-destroy d))))
               (cut (&rest args) (declare (ignore args))
                    (text-buffer-cut-clipboard (text-view-buffer text-view) (get-clipboard "CLIPBOARD") t))
               (copy (&rest args) (declare (ignore args))
                     (text-buffer-copy-clipboard (text-view-buffer text-view) (get-clipboard "CLIPBOARD")))
               (paste (&rest args) (declare (ignore args))
                      (text-buffer-paste-clipboard (text-view-buffer text-view) (get-clipboard "CLIPBOARD")))
               (cb-delete (&rest args) (declare (ignore args))
                          (let ((buffer (text-view-buffer text-view)))
                            (multiple-value-bind (i1 i2) (text-buffer-get-selection-bounds buffer)
                              (when (and i1 i2)
                                (text-buffer-delete buffer i1 i2)))))
               (about (&rest args) (declare (ignore args))
                      (let ((d (make-instance 'about-dialog
                                              :program-name "Lisp Gtk+ Binding Demo Text Editor"
                                              :version (format nil "0.0.0.1 ~A" #\GREEK_SMALL_LETTER_ALPHA)
                                              :authors '("Kalyanov Dmitry")
                                              :license "Public Domain"
                                              :logo-icon-name "accessories-text-editor")))
                        (dialog-run d)
                        (object-destroy d)))
               (quit (&rest args) (declare (ignore args)) (object-destroy window))
               (cb-eval (&rest args) (declare (ignore args))
                        (let ((buffer (text-view-buffer text-view)))
                          (multiple-value-bind (i1 i2) (text-buffer-get-selection-bounds buffer)
                            (when (and i1 i2)
                              (with-gtk-message-error-handler
                                (let* ((text (text-buffer-slice buffer i1 i2))
                                       (value (eval (read-from-string text)))
                                       (value-str (format nil "~A" value))
                                       (pos (max (text-iter-offset i1) (text-iter-offset i2))))
                                  (text-buffer-insert buffer " => " :position (text-buffer-get-iter-at-offset buffer pos))
                                  (incf pos (length " => "))
                                  (text-buffer-insert buffer value-str :position (text-buffer-get-iter-at-offset buffer pos)))))))))
        (builder-connect-signals-simple builder `(("new" ,#'new)
                                                  ("open" ,#'cb-open)
                                                  ("save" ,#'save)
                                                  ("save-as" ,#'save-as)
                                                  ("cut" ,#'cut)
                                                  ("copy" ,#'copy)
                                                  ("paste" ,#'paste)
                                                  ("delete" ,#'cb-delete)
                                                  ("about" ,#'about)
                                                  ("quit" ,#'quit)
                                                  ("eval" ,#'cb-eval)))
        (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
        (g-signal-connect (text-view-buffer text-view) "changed" (lambda (b) (declare (ignore b)) (setf modified-p t) (set-properties)))
        (widget-show window)))))

(defun demo-class-browser ()
  "Show slots of a given class"
  (let ((output *standard-output*))
    (with-main-loop
        (let* ((window (make-instance 'gtk-window
                                      :window-position :center
                                      :title "Class Browser"
                                      :default-width 400
                                      :default-height 600))
               (search-entry (make-instance 'entry))
               (search-button (make-instance 'button :label "Search"))
               (scroll (make-instance 'scrolled-window
                                      :hscrollbar-policy :automatic
                                      :vscrollbar-policy :automatic))
               (slots-model (make-instance 'array-list-store))
               (slots-list (make-instance 'tree-view :model slots-model)))
          (let ((v-box (make-instance 'v-box))
                (search-box (make-instance 'h-box)))
            (container-add window v-box)
            (box-pack-start v-box search-box :expand nil)
            (box-pack-start search-box search-entry)
            (box-pack-start search-box search-button :expand nil)
            (box-pack-start v-box scroll)
            (container-add scroll slots-list))
          (store-add-column slots-model "gchararray"
                            (lambda (slot)
                              (format nil "~S" (closer-mop:slot-definition-name slot))))
          (let ((col (make-instance 'tree-view-column :title "Slot name"))
                (cr (make-instance 'cell-renderer-text)))
            (tree-view-column-pack-start col cr)
            (tree-view-column-add-attribute col cr "text" 0)
            (tree-view-append-column slots-list col))
          (labels ((display-class-slots (class)
                     (format output "Displaying ~A~%" class)
                     (loop
                        repeat (store-items-count slots-model)
                        do (store-remove-item slots-model (store-item slots-model 0)))
                     (closer-mop:finalize-inheritance class)
                     (loop
                        for slot in (closer-mop:class-slots class)
                        do (store-add-item slots-model slot)))
                   (on-search-clicked (button)
                     (declare (ignore button))
                     (with-gtk-message-error-handler
                         (let* ((class-name (read-from-string (entry-text search-entry)))
                                (class (find-class class-name)))
                           (display-class-slots class)))))
            (g-signal-connect search-button "clicked" #'on-search-clicked))
          (widget-show window)))))

(defun make-tree-from-sexp (l)
  (setf l (if (listp l) l (list l)))
  (let ((node (make-tree-node :item (make-tvi :title (format nil "~S" (first l))
                                              :value (format nil "~S" (class-of (first l)))))))
    (iter (for child in (rest l))
          (tree-node-insert-at node (make-tree-from-sexp child) (length (tree-node-children node))))
    node))

(defun demo-treeview-tree ()
  "Advanced demo: show s-expression tree structure"
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :type :toplevel :title "Treeview (tree)"))
           (model (make-instance 'tree-lisp-store))
           (scroll (make-instance 'scrolled-window :hscrollbar-policy :automatic :vscrollbar-policy :automatic))
           (tree-view (make-instance 'tree-view :headers-visible t :width-request 300 :height-request 400 :rules-hint t))
           (h-box (make-instance 'h-box))
           (v-box (make-instance 'v-box))
           (entry (make-instance 'entry))
           (button (make-instance 'button :label "Display")))
      (tree-lisp-store-add-column model "gchararray" #'tvi-title)
      (tree-lisp-store-add-column model "gchararray" #'tvi-value)
      (tree-node-insert-at (tree-lisp-store-root model)
                           (make-tree-from-sexp '(lambda (object &rest initargs &key &allow-other-keys)
                                                  (* 1 2)
                                                  (- 3 4)))
                           0)
      (setf (tree-view-model tree-view) model
            (tree-view-tooltip-column tree-view) 0)
      (connect-signal tree-view "row-activated" (lambda (tv path column)
                                                  (declare (ignore tv column))
                                                  (format t "You clicked on row ~A~%" (tree-path-indices path))))
      (connect-signal button "clicked" (lambda (b)
                                         (declare (ignore b))
                                         (let ((object (read-from-string (entry-text entry))))
                                           (tree-node-remove-at (tree-lisp-store-root model) 0)
                                           (tree-node-insert-at (tree-lisp-store-root model)
                                                                (make-tree-from-sexp object)
                                                                0))))
      (container-add window v-box)
      (box-pack-start v-box h-box :expand nil)
      (box-pack-start h-box entry)
      (box-pack-start h-box button :expand nil)
      (box-pack-start v-box scroll)
      (container-add scroll tree-view)
      (let ((column (make-instance 'tree-view-column :title "Value" :sort-column-id 0))
            (renderer (make-instance 'cell-renderer-text :text "A text")))
        (tree-view-column-pack-start column renderer)
        (tree-view-column-add-attribute column renderer "text" 0)
        (tree-view-append-column tree-view column)
        (print (tree-view-column-tree-view column))
        (print (tree-view-column-cell-renderers column)))
      (let ((column (make-instance 'tree-view-column :title "Type"))
            (renderer (make-instance 'cell-renderer-text :text "A text")))
        (tree-view-column-pack-start column renderer)
        (tree-view-column-add-attribute column renderer "text" 1)
        (tree-view-append-column tree-view column)
        (print (tree-view-column-tree-view column))
        (print (tree-view-column-cell-renderers column)))
      (widget-show window))))

(defclass custom-window (gtk-window)
  ((label :initform (make-instance 'label :label "A label text") :reader custom-window-label)
   (button :initform (make-instance 'button :label "Click me!") :reader custom-window-button))
  (:metaclass gobject-class)
  (:default-initargs :title "Custom window with default initargs" :default-width 320 :default-height 240))

(defun custom-window-label-text (w)
  (label-label (custom-window-label w)))

(defun (setf custom-window-label-text) (new-value w)
  (setf (label-label (custom-window-label w)) new-value))

(defmethod initialize-instance :after ((w custom-window) &key &allow-other-keys)
  (let ((box (make-instance 'v-box)))
    (box-pack-start box (custom-window-label w))
    (box-pack-start box (custom-window-button w) :expand nil)
    (container-add w box))
  (connect-signal (custom-window-button w) "clicked" (lambda (b)
                                                       (declare (ignore b))
                                                       (custom-window-button-clicked w))))

(defun custom-window-button-clicked (w)
  (setf (custom-window-label-text w)
        (format nil "Now is: ~A~%" (get-internal-run-time))))

(defun test-custom-window ()
  "Simple test of non-GObject subclass of GtkWindow"
  (within-main-loop
    (let ((w (make-instance 'custom-window)))
      (widget-show w))))

(defun test-assistant ()
  "Simple test of GtkAssistant wizard"
  (let ((output *standard-output*))
    (within-main-loop
      (let ((d (make-instance 'assistant :title "Username wizard"))
            (p-1 (make-instance 'h-box))
            (entry (make-instance 'entry))
            (p-2 (make-instance 'label :label "Click Apply to close this wizard")))
        (box-pack-start p-1 (make-instance 'label :label "Enter your name:") :expand nil)
        (box-pack-start p-1 entry)
        (assistant-append-page d p-1)
        (assistant-append-page d p-2)
        (setf (assistant-child-title d p-1) "Username wizard"
              (assistant-child-title d p-2) "Username wizard"
              (assistant-child-complete d p-1) nil
              (assistant-child-complete d p-2) t
              (assistant-child-page-type d p-1) :intro
              (assistant-child-page-type d p-2) :confirm
              (assistant-forward-page-function d) (lambda (i)
                                                    (format output "(assistant-forward-page-function ~A)~%" i)
                                                    (ecase i
                                                      (0 1)
                                                      (1 -1))))
        (connect-signal entry "notify::text" (lambda (object pspec)
                                               (declare (ignore object pspec))
                                               (setf (assistant-child-complete d p-1)
                                                     (plusp (length (entry-text entry))))))
        (let ((w (make-instance 'label :label "A label in action area")))
          (widget-show w)
          (assistant-add-action-widget d w))
        (connect-signal d "cancel" (lambda (assistant)
                                     (declare (ignore assistant))
                                     (object-destroy d)
                                     (format output "Canceled~%")))
        (connect-signal d "close" (lambda (assistant)
                                    (declare (ignore assistant))
                                    (object-destroy d)
                                    (format output "Thank you, ~A~%" (entry-text entry))))
        (connect-signal d "prepare" (lambda (assistant page-widget)
                                      (declare (ignore assistant page-widget))
                                      (format output "Assistant ~A has ~A pages and is on ~Ath page~%"
                                              d (assistant-n-pages d) (assistant-current-page d))))
        (widget-show d)))))

(defun test-entry-completion ()
  "Not working example of GtkEntryCompletion"
  (within-main-loop
    (let* ((w (make-instance 'gtk-window))
           (model (make-instance 'tree-lisp-store)))
      (tree-lisp-store-add-column model "gchararray" #'identity)
      (tree-node-insert-at (tree-lisp-store-root model) (make-tree-node :item "Monday") 0)
      (tree-node-insert-at (tree-lisp-store-root model) (make-tree-node :item "Tuesday") 0)
      (tree-node-insert-at (tree-lisp-store-root model) (make-tree-node :item "Wednesday") 0)
      (tree-node-insert-at (tree-lisp-store-root model) (make-tree-node :item "Thursday") 0)
      (tree-node-insert-at (tree-lisp-store-root model) (make-tree-node :item "Friday") 0)
      (tree-node-insert-at (tree-lisp-store-root model) (make-tree-node :item "Saturday") 0)
      (tree-node-insert-at (tree-lisp-store-root model) (make-tree-node :item "Sunday") 0)
      (let* ((completion (make-instance 'entry-completion :model model :text-column 0))
             (e (make-instance 'entry :completion completion)))
        (setf (entry-completion-text-column completion) 0)
        (container-add w e))
      (widget-show w))))

(defun demo-all ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :title "cl-gtk2-gtk demo"
                                  :window-position :center
                                  :default-width 500
                                  :default-height 500))
           (scrolled (make-instance 'scrolled-window
                                    :hscrollbar-policy :automatic
                                    :vscrollbar-policy :automatic))
           (viewport (make-instance 'viewport))
           (v-box-buttons (make-instance 'v-box))
           (v-box-top (make-instance 'v-box)))
      (container-add window v-box-top)
      (box-pack-start v-box-top (make-instance 'label :label "These are the demos of cl-gtk2-gtk:") :expand nil)
      (box-pack-start v-box-top scrolled)
      (container-add scrolled viewport)
      (container-add viewport v-box-buttons)
      (iter (for s in-package :gtk-demo :external-only t)
            (for fn = (fdefinition s))
            (unless fn (next-iteration))
            (when (eq s 'gtk-demo:demo-all) (next-iteration))
            (for docstring = (documentation fn t))
            (for description = (format nil "~A~@[~%~A~]" (string-downcase (symbol-name s)) docstring))
            (for label = (make-instance 'label :label description :justify :center))
            (for button = (make-instance 'button))
            (container-add button label)
            (connect-signal button "clicked"
                            (let ((fn fn))
                              (lambda (b)
                                (declare (ignore b))
                                (funcall fn))))
            (box-pack-start v-box-buttons button :expand nil))
      (widget-show window))))

(defun test-ui-markup ()
  (within-main-loop
    (let ((label (make-instance 'label :label "Hello!")))
      (let-ui (gtk-window :type :toplevel
                          :position :center
                          :title "Hello, world!"
                          :default-width 300
                          :default-height 400
                          :var w
                          (v-box
                           (:expr label) :expand nil
                           (scrolled-window
                            :hscrollbar-policy :automatic
                            :vscrollbar-policy :automatic
                            :shadow-type :etched-in
                            (text-view :var tv))
                           (h-box
                            (label :label "Insert:") :expand nil
                            (entry :var entry)
                            (button :label "gtk-ok" :use-stock t :var btn) :expand nil)
                           :expand nil
                           (label :label "Table packing")
                           :expand nil
                           (table
                            :n-columns 2
                            :n-rows 2
                            (label :label "2 x 1") :left 0 :right 2 :top 0 :bottom 1
                            (label :label "1 x 1") :left 0 :right 1 :top 1 :bottom 2
                            (label :label "1 x 1") :left 1 :right 2 :top 1 :bottom 2)))
        (connect-signal btn "clicked"
                        (lambda (b)
                          (declare (ignore b))
                          (text-buffer-insert (text-view-buffer tv)
                                              (entry-text entry))))
        (widget-show w)))))

(defun test-list-store ()
  "Demonstrates usage of list store"
  (within-main-loop
    (let-ui (gtk-window
             :type :toplevel
             :title "GtkListStore"
             :default-width 600
             :default-height 400
             :var w
             (v-box
              (label :label "A GtkListStore") :expand nil
              (scrolled-window
               :hscrollbar-policy :automatic
               :vscrollbar-policy :automatic
               (tree-view :var tv))))
      (let ((l (make-instance 'list-store :column-types '("gint" "gchararray"))))
        (iter (for i from 0 below 100)
              (for n = (random 10000000))
              (for s = (format nil "~R" n))
              (list-store-insert-with-values l i n s))
        (setf (tree-view-model tv) l)
        (let ((column (make-instance 'tree-view-column :title "Number" :sort-column-id 0))
              (renderer (make-instance 'cell-renderer-text :text "A text")))
          (tree-view-column-pack-start column renderer)
          (tree-view-column-add-attribute column renderer "text" 0)
          (tree-view-append-column tv column))
        (let ((column (make-instance 'tree-view-column :title "As string" :sort-column-id 1))
              (renderer (make-instance 'cell-renderer-text :text "A text")))
          (tree-view-column-pack-start column renderer)
          (tree-view-column-add-attribute column renderer "text" 1)
          (tree-view-append-column tv column))
        (connect-signal tv "row-activated"
                        (lambda (w path column)
                          (declare (ignore w column))
                          (let* ((iter (tree-model-iter-by-path l path))
                                 (n (tree-model-value l iter 0))
                                 (dialog (make-instance 'message-dialog
                                                        :title "Clicked"
                                                        :text (format nil "Number ~A was clicked" n)
                                                        :buttons :ok)))
                            (dialog-run dialog)
                            (object-destroy dialog)))))
      (widget-show w))))

(defun test-tree-store ()
  "Demonstrates usage of tree store"
  (within-main-loop
    (let-ui (gtk-window
             :type :toplevel
             :title "GtkListStore"
             :default-width 600
             :default-height 400
             :var w
             (v-box
              (label :label "A GtkListStore") :expand nil
              (scrolled-window
               :hscrollbar-policy :automatic
               :vscrollbar-policy :automatic
               (tree-view :var tv))))
      (let ((l (make-instance 'tree-store :column-types '("gint" "gchararray"))))
        (iter (for i from 0 below 100)
              (for n = (random 10000000))
              (for s = (format nil "~R" n))
              (for it = (tree-store-insert-with-values l nil i n s))
              (iter (for j from 0 below 10)
                    (for n2 = (random 10000000))
                    (for s2 = (format nil "~R" n2))
                    (tree-store-insert-with-values l it j n2 s2)))
        (setf (tree-view-model tv) l)
        (let ((column (make-instance 'tree-view-column :title "Number" :sort-column-id 0))
              (renderer (make-instance 'cell-renderer-text :text "A text")))
          (tree-view-column-pack-start column renderer)
          (tree-view-column-add-attribute column renderer "text" 0)
          (tree-view-append-column tv column))
        (let ((column (make-instance 'tree-view-column :title "As string" :sort-column-id 1))
              (renderer (make-instance 'cell-renderer-text :text "A text")))
          (tree-view-column-pack-start column renderer)
          (tree-view-column-add-attribute column renderer "text" 1)
          (tree-view-append-column tv column))
        (connect-signal tv "row-activated"
                        (lambda (w path column)
                          (declare (ignore w column))
                          (let* ((iter (tree-model-iter-by-path l path))
                                 (n (tree-model-value l iter 0))
                                 (dialog (make-instance 'message-dialog
                                                        :title "Clicked"
                                                        :text (format nil "Number ~A was clicked" n)
                                                        :buttons :ok)))
                            (dialog-run dialog)
                            (object-destroy dialog)))))
      (widget-show w))))

(defun test-gdk-expose (gdk-window)
  (let* ((gc (graphics-context-new gdk-window)))
    (multiple-value-bind (w h) (drawable-get-size gdk-window)
      (setf (graphics-context-rgb-bg-color gc) (make-color :red 0 :green 0 :blue 0))
      (draw-polygon gdk-window gc t (list (make-point :x 0 :y 0)
                                          (make-point :x (truncate w 2) :y 0)
                                          (make-point :x w :y (truncate h 2))
                                          (make-point :x w :y h)
                                          (make-point :x (truncate w 2) :y h)
                                          (make-point :x 0 :y (truncate h 2))))
      (setf (graphics-context-rgb-fg-color gc) (make-color :red 65535 :green 0 :blue 0))
      (draw-point gdk-window gc 20 10)
      (setf (graphics-context-rgb-fg-color gc) (make-color :red 0 :green 65535 :blue 0))
      (draw-points gdk-window gc (list (make-point :x 15 :y 20) (make-point :x 35 :y 40)))
      (setf (graphics-context-rgb-fg-color gc) (make-color :red 0 :green 0 :blue 65535))
      (draw-line gdk-window gc 60 30 40 50)
      (setf (graphics-context-rgb-fg-color gc) (make-color :red 65535 :green 65535 :blue 0))
      (draw-lines gdk-window gc (list (make-point :x 10 :y 30) (make-point :x 15 :y 40)
                                      (make-point :x 15 :y 50) (make-point :x 10 :y 56)))
      (setf (graphics-context-rgb-fg-color gc) (make-color :red 0 :green 65535 :blue 65535))
      (draw-segments gdk-window gc (list (make-segment :x1 35 :y1 35 :x2 55 :y2 35)
                                         (make-segment :x1 65 :y1 35 :x2 43 :y2 17)))
      (setf (graphics-context-rgb-fg-color gc) (make-color :red 65535 :green 0 :blue 65535)
            (graphics-context-rgb-bg-color gc) (make-color :red 32767 :green 0 :blue 32767))
      (draw-arc gdk-window gc nil 70 30 75 50 (* 64 75) (* 64 200))
      (draw-polygon gdk-window gc nil (list (make-point :x 20 :y 40)
                                            (make-point :x 30 :y 50)
                                            (make-point :x 40 :y 70)
                                            (make-point :x 30 :y 80)
                                            (make-point :x 10 :y 55)))
      (setf (graphics-context-rgb-fg-color gc) (make-color :red 16384 :green 16384 :blue 65535))
      (draw-trapezoids gdk-window gc (list (make-trapezoid :y1 50.0d0 :y2 70.0d0
                                                           :x11 30.0d0 :x12 45.0d0
                                                           :x21 70.0d0 :x22 50.0d0))))))

(defun test-gdk ()
  "Test various gdk primitives"
  (within-main-loop
    (let ((window (make-instance 'gtk-window :type :toplevel :app-paintable t)))
      (g-signal-connect window "destroy" (lambda (widget)
                                           (declare (ignore widget))
                                           (leave-gtk-main)))
      (g-signal-connect window "destroy" (lambda (widget)
                                           (declare (ignore widget))
                                           (leave-gtk-main)))
      (g-signal-connect window "expose-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (test-gdk-expose (widget-window window))))
      (g-signal-connect window "configure-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (widget-queue-draw window)))
      (widget-show window)
      (push :pointer-motion-mask (gdk-window-events (widget-window window))))))

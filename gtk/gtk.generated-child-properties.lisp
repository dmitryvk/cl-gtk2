(in-package :gtk)
(define-child-property "GtkAssistant" assistant-child-page-type "page-type"
                       "GtkAssistantPageType" t t t)

(define-child-property "GtkAssistant" assistant-child-title "title"
                       "gchararray" t t t)

(define-child-property "GtkAssistant" assistant-child-header-image
                       "header-image" "GdkPixbuf" t t t)

(define-child-property "GtkAssistant" assistant-child-sidebar-image
                       "sidebar-image" "GdkPixbuf" t t t)

(define-child-property "GtkAssistant" assistant-child-complete "complete"
                       "gboolean" t t t)

(define-child-property "GtkMenu" menu-child-left-attach "left-attach" "gint" t
                       t t)

(define-child-property "GtkMenu" menu-child-right-attach "right-attach" "gint"
                       t t t)

(define-child-property "GtkMenu" menu-child-top-attach "top-attach" "gint" t t
                       t)

(define-child-property "GtkMenu" menu-child-bottom-attach "bottom-attach"
                       "gint" t t t)

(define-child-property "GtkRecentChooserMenu"
                       recent-chooser-menu-child-left-attach "left-attach"
                       "gint" t t t)

(define-child-property "GtkRecentChooserMenu"
                       recent-chooser-menu-child-right-attach "right-attach"
                       "gint" t t t)

(define-child-property "GtkRecentChooserMenu"
                       recent-chooser-menu-child-top-attach "top-attach" "gint"
                       t t t)

(define-child-property "GtkRecentChooserMenu"
                       recent-chooser-menu-child-bottom-attach "bottom-attach"
                       "gint" t t t)

(define-child-property "GtkNotebook" notebook-child-tab-label "tab-label"
                       "gchararray" t t t)

(define-child-property "GtkNotebook" notebook-child-menu-label "menu-label"
                       "gchararray" t t t)

(define-child-property "GtkNotebook" notebook-child-position "position" "gint"
                       t t t)

(define-child-property "GtkNotebook" notebook-child-tab-expand "tab-expand"
                       "gboolean" t t t)

(define-child-property "GtkNotebook" notebook-child-tab-fill "tab-fill"
                       "gboolean" t t t)

(define-child-property "GtkNotebook" notebook-child-tab-pack "tab-pack"
                       "GtkPackType" t t t)

(define-child-property "GtkNotebook" notebook-child-reorderable "reorderable"
                       "gboolean" t t t)

(define-child-property "GtkNotebook" notebook-child-detachable "detachable"
                       "gboolean" t t t)

(define-child-property "GtkBox" box-child-expand "expand" "gboolean" t t t)

(define-child-property "GtkBox" box-child-fill "fill" "gboolean" t t t)

(define-child-property "GtkBox" box-child-padding "padding" "guint" t t t)

(define-child-property "GtkBox" box-child-pack-type "pack-type" "GtkPackType" t
                       t t)

(define-child-property "GtkBox" box-child-position "position" "gint" t t t)

(define-child-property "GtkButtonBox" button-box-child-expand "expand"
                       "gboolean" t t t)

(define-child-property "GtkButtonBox" button-box-child-fill "fill" "gboolean" t
                       t t)

(define-child-property "GtkButtonBox" button-box-child-padding "padding"
                       "guint" t t t)

(define-child-property "GtkButtonBox" button-box-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkButtonBox" button-box-child-position "position"
                       "gint" t t t)

(define-child-property "GtkButtonBox" button-box-child-secondary "secondary"
                       "gboolean" t t t)

(define-child-property "GtkHButtonBox" h-button-box-child-expand "expand"
                       "gboolean" t t t)

(define-child-property "GtkHButtonBox" h-button-box-child-fill "fill"
                       "gboolean" t t t)

(define-child-property "GtkHButtonBox" h-button-box-child-padding "padding"
                       "guint" t t t)

(define-child-property "GtkHButtonBox" h-button-box-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkHButtonBox" h-button-box-child-position "position"
                       "gint" t t t)

(define-child-property "GtkHButtonBox" h-button-box-child-secondary "secondary"
                       "gboolean" t t t)

(define-child-property "GtkVButtonBox" v-button-box-child-expand "expand"
                       "gboolean" t t t)

(define-child-property "GtkVButtonBox" v-button-box-child-fill "fill"
                       "gboolean" t t t)

(define-child-property "GtkVButtonBox" v-button-box-child-padding "padding"
                       "guint" t t t)

(define-child-property "GtkVButtonBox" v-button-box-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkVButtonBox" v-button-box-child-position "position"
                       "gint" t t t)

(define-child-property "GtkVButtonBox" v-button-box-child-secondary "secondary"
                       "gboolean" t t t)

(define-child-property "GtkVBox" v-box-child-expand "expand" "gboolean" t t t)

(define-child-property "GtkVBox" v-box-child-fill "fill" "gboolean" t t t)

(define-child-property "GtkVBox" v-box-child-padding "padding" "guint" t t t)

(define-child-property "GtkVBox" v-box-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkVBox" v-box-child-position "position" "gint" t t t)

(define-child-property "GtkColorSelection" color-selection-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkColorSelection" color-selection-child-fill "fill"
                       "gboolean" t t t)

(define-child-property "GtkColorSelection" color-selection-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkColorSelection" color-selection-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkColorSelection" color-selection-child-position
                       "position" "gint" t t t)

(define-child-property "GtkFileChooserWidget" file-chooser-widget-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkFileChooserWidget" file-chooser-widget-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkFileChooserWidget" file-chooser-widget-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkFileChooserWidget"
                       file-chooser-widget-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkFileChooserWidget"
                       file-chooser-widget-child-position "position" "gint" t t
                       t)

(define-child-property "GtkFontSelection" font-selection-child-expand "expand"
                       "gboolean" t t t)

(define-child-property "GtkFontSelection" font-selection-child-fill "fill"
                       "gboolean" t t t)

(define-child-property "GtkFontSelection" font-selection-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkFontSelection" font-selection-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkFontSelection" font-selection-child-position
                       "position" "gint" t t t)

(define-child-property "GtkGammaCurve" gamma-curve-child-expand "expand"
                       "gboolean" t t t)

(define-child-property "GtkGammaCurve" gamma-curve-child-fill "fill" "gboolean"
                       t t t)

(define-child-property "GtkGammaCurve" gamma-curve-child-padding "padding"
                       "guint" t t t)

(define-child-property "GtkGammaCurve" gamma-curve-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkGammaCurve" gamma-curve-child-position "position"
                       "gint" t t t)

(define-child-property "GtkRecentChooserWidget"
                       recent-chooser-widget-child-expand "expand" "gboolean" t
                       t t)

(define-child-property "GtkRecentChooserWidget"
                       recent-chooser-widget-child-fill "fill" "gboolean" t t t)

(define-child-property "GtkRecentChooserWidget"
                       recent-chooser-widget-child-padding "padding" "guint" t
                       t t)

(define-child-property "GtkRecentChooserWidget"
                       recent-chooser-widget-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkRecentChooserWidget"
                       recent-chooser-widget-child-position "position" "gint" t
                       t t)

(define-child-property "GtkHBox" h-box-child-expand "expand" "gboolean" t t t)

(define-child-property "GtkHBox" h-box-child-fill "fill" "gboolean" t t t)

(define-child-property "GtkHBox" h-box-child-padding "padding" "guint" t t t)

(define-child-property "GtkHBox" h-box-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkHBox" h-box-child-position "position" "gint" t t t)

(define-child-property "GtkFileChooserButton" file-chooser-button-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkFileChooserButton" file-chooser-button-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkFileChooserButton" file-chooser-button-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkFileChooserButton"
                       file-chooser-button-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkFileChooserButton"
                       file-chooser-button-child-position "position" "gint" t t
                       t)

(define-child-property "GtkStatusbar" statusbar-child-expand "expand"
                       "gboolean" t t t)

(define-child-property "GtkStatusbar" statusbar-child-fill "fill" "gboolean" t
                       t t)

(define-child-property "GtkStatusbar" statusbar-child-padding "padding" "guint"
                       t t t)

(define-child-property "GtkStatusbar" statusbar-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkStatusbar" statusbar-child-position "position"
                       "gint" t t t)

(define-child-property "GtkFixed" fixed-child-x "x" "gint" t t t)

(define-child-property "GtkFixed" fixed-child-y "y" "gint" t t t)

(define-child-property "GtkPaned" paned-child-resize "resize" "gboolean" t t t)

(define-child-property "GtkPaned" paned-child-shrink "shrink" "gboolean" t t t)

(define-child-property "GtkHPaned" h-paned-child-resize "resize" "gboolean" t t
                       t)

(define-child-property "GtkHPaned" h-paned-child-shrink "shrink" "gboolean" t t
                       t)

(define-child-property "GtkVPaned" v-paned-child-resize "resize" "gboolean" t t
                       t)

(define-child-property "GtkVPaned" v-paned-child-shrink "shrink" "gboolean" t t
                       t)

(define-child-property "GtkLayout" layout-child-x "x" "gint" t t t)

(define-child-property "GtkLayout" layout-child-y "y" "gint" t t t)

(define-child-property "GtkTable" table-child-left-attach "left-attach" "guint"
                       t t t)

(define-child-property "GtkTable" table-child-right-attach "right-attach"
                       "guint" t t t)

(define-child-property "GtkTable" table-child-top-attach "top-attach" "guint" t
                       t t)

(define-child-property "GtkTable" table-child-bottom-attach "bottom-attach"
                       "guint" t t t)

(define-child-property "GtkTable" table-child-x-options "x-options"
                       "GtkAttachOptions" t t t)

(define-child-property "GtkTable" table-child-y-options "y-options"
                       "GtkAttachOptions" t t t)

(define-child-property "GtkTable" table-child-x-padding "x-padding" "guint" t t
                       t)

(define-child-property "GtkTable" table-child-y-padding "y-padding" "guint" t t
                       t)

(define-child-property "GtkToolbar" toolbar-child-expand "expand" "gboolean" t
                       t t)

(define-child-property "GtkToolbar" toolbar-child-homogeneous "homogeneous"
                       "gboolean" t t t)


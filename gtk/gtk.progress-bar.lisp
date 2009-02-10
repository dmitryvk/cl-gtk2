(in-package :gtk)

(defcfun (progress-bar-pulse "gtk_progress_bar_pulse") :void
  (progress-bar (g-object progress-bar)))

(export 'progress-bar-pulse)

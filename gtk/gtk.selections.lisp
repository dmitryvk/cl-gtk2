(in-package :gtk)

(define-g-boxed-cstruct target-entry "GtkTargetEntry"
  (target :string :initform 0)
  (flags target-flags :initform 0)
  (info :uint :initform 0))

(export (boxed-related-symbols 'target-entry))


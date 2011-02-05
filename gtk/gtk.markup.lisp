(in-package :gtk)

(defcfun |g_markup_escape_text| :string
  (raw  :string)
  (size :int))

(defun markup-escape-text (raw)
  (cffi:with-foreign-string (s raw)
    (|g_markup_escape_text| s (length raw))))

(export 'markup-escape-text)

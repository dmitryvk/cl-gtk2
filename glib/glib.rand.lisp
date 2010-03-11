(in-package :glib)

(defcfun (random-set-seed "g_random_set_seed") :void
  (seed :uint32))

(export 'random-set-seed)

(defcfun (random-int "g_random_int") :uint32)

(export 'random-int)

(defcfun (random-int-range "g_random_int_range") :int32
  (begin :int32)
  (end :int32))

(export 'random-int-range)

(defun random-boolean ()
  (logtest (random-int) #X8000))

(export 'random-boolean)

(defcfun (random-double "g_random_double") :double)

(export 'random-double)

(defcfun (random-double-range "g_random_double_range") :double
  (begin :double)
  (end :double))

(export 'random-double-range)


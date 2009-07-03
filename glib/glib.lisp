(defpackage :glib
  (:use :cl :cffi :iter)
  (:export #:at-init
           #:gsize
           #:gssize
           #:goffset
           #:*glib-major-version*
           #:*glib-minor-version*
           #:*glib-micro-version*
           #:*glib-binary-age*
           #:*glib-interface-age*
           #:g-free
           #:glist
           #:gstrv
           #:g-malloc
           #:g-strdup
           #:g-string
           #:gslist
           #:g-quark
           #:+g-priority-high+
           #:+g-priority-default+
           #:+g-priority-high-idle+
           #:+g-priority-default-idle+
           #:+g-priority-low+
           #:g-idle-add-full))

(in-package :glib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *initializers* nil)
  (defun register-initializer (fn)
    (setf *initializers* (nconc *initializers* (list fn)))))

(defun run-initializers ()
  (iter (for fn in *initializers*)
        (funcall fn)))

(defmacro at-init (&body body)
  `(progn (register-initializer (lambda () ,@body))
          ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library glib
    (:unix (:or "libglib-2.0.so.0" "libglib-2.0.so"))
    (t "libglib-2.0")))

(use-foreign-library glib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library gthread
    (:unix (:or "libgthread-2.0.so.0"  "libgthread-2.0.so"))
    (t "libgthread-2.0")))

(use-foreign-library gthread)

;;
;; Glib Fundamentals
;;

;;
;; Fundamentals - Basic types
;;


;; TODO: not sure about these: for amd64 they are ok
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cond
    ((cffi-features:cffi-feature-p :x86-64) (defctype gsize :uint64))
    ((cffi-features:cffi-feature-p :x86) (defctype gsize :ulong))
    (t (error "Can not define 'gsize', unknown CPU architecture (known are x86 and x86-64)"))))

(defctype gssize :long)

(defctype goffset :uint64)


;;
;; Fundamentals - Version information
;;

(defcvar (*glib-major-version* "glib_major_version" :read-only t :library glib) :uint)
(defcvar (*glib-minor-version* "glib_minor_version" :read-only t :library glib) :uint)
(defcvar (*glib-micro-version* "glib_micro_version" :read-only t :library glib) :uint)
(defcvar (*glib-binary-age* "glib_binary_age" :read-only t :library glib) :uint)
(defcvar (*glib-interface-age* "glib_interface_age" :read-only t :library glib) :uint)

;;
;; Omitted:
;; Limits of Basic Types, Standard Macros, Type Conversion Macros, Byte Order Macros, 
;; Numerical Definitions, Miscellaneous Macros, Atomic operations
;;

;; Core Application Support - The Main Event Loop

(defcstruct g-main-loop)
(defcstruct g-main-context)
(defcstruct g-source)
(defcstruct g-source-funcs
  (prepare :pointer)
  (check :pointer)
  (dispatch :pointer)
  (finalize :pointer)
  (closure-callback :pointer)
  (closure-marshal :pointer))
(defcstruct g-source-callback-funcs
  (ref :pointer)
  (unref :pointer)
  (get :pointer))
(defcstruct g-cond)
(defcstruct g-mutex)

(defcstruct g-poll-fd
  (fd :int) ;; TODO: #if defined (G_OS_WIN32) && GLIB_SIZEOF_VOID_P == 8
  (events :ushort)
  (revent :ushort))

(defcstruct g-time-val
  (seconds :long)
  (microseconds :long))

(defcstruct g-thread)

(defcfun (g-main-loop-new "g_main_loop_new" :library glib) (:pointer g-main-loop)
  (context (:pointer g-main-context))
  (is-running :boolean))

(defcfun (g-main-loop-ref "g_main_loop_ref" :library glib) (:pointer g-main-loop)
  (loop (:pointer g-main-loop)))

(defcfun (g-main-loop-unref "g_main_loop_unref" :library glib) (:pointer g-main-loop)
  (loop (:pointer g-main-loop)))

(defcfun (g-main-loop-run "g_main_loop_run" :library glib) :void
  (loop (:pointer g-main-loop)))

(defcfun (g-main-loop-quit "g_main_loop_quit" :library glib) :void
  (loop (:pointer g-main-loop)))

(defcfun (g-main-loop-is-running "g_main_loop_is_running" :library glib) :boolean
  (loop (:pointer g-main-loop)))

(defcfun (g-main-loop-get-context "g_main_loop_get_context" :library glib) (:pointer g-main-context)
  (loop (:pointer g-main-loop)))

(defconstant +g-priority-high+ -100)
(defconstant +g-priority-default+ 0)
(defconstant +g-priority-high-idle+ 100)
(defconstant +g-priority-default-idle+ 200)
(defconstant +g-priority-low+ 300)

(defcfun (g-main-context-new "g_main_context_new" :library glib) (:pointer g-main-context))

(defcfun (g-main-context-ref "g_main_context_ref" :library glib) (:pointer g-main-context)
  (context (:pointer g-main-context)))

(defcfun (g-main-context-unref "g_main_context_unref" :library glib) (:pointer g-main-context)
  (context (:pointer g-main-context)))

(defcfun (g-main-context-default "g_main_context_default" :library glib) (:pointer g-main-context))

(defcfun (g-main-context-iteration "g_main_context_iteration" :library glib) :boolean
  (context (:pointer g-main-context))
  (may-block :boolean))

(defcfun (g-main-context-pending "g_main_context_pending" :library glib) :boolean
  (context (:pointer g-main-context)))

(defcfun (g-main-context-find-source-by-id "g_main_context_find_source_by_id" :library glib) (:pointer g-source)
  (context (:pointer g-main-context))
  (source-id :uint))

(defcfun (g-main-context-find-source-by-user-data "g_main_context_find_source_by_user_data" :library glib) (:pointer g-source)
  (context (:pointer g-main-context))
  (user-data :pointer))

(defcfun (g-main-context-find-source-by-funcs-user-data "g_main_context_find_source_by_funcs_user_data" :library glib) (:pointer g-source)
  (context (:pointer g-main-context))
  (funcs (:pointer g-source-funcs))
  (user-data :pointer))

(defcfun (g-main-context-wakeup "g_main_context_wakeup" :library glib) :void
  (context (:pointer g-main-context)))

(defcfun (g-main-context-acquire "g_main_context_acquire" :library glib) :boolean
  (context (:pointer g-main-context)))

(defcfun (g-main-context-release "g_main_context_release" :library glib) :void
  (context (:pointer g-main-context)))

(defcfun (g-main-context-is-owner "g_main_context_is_owner" :library glib) :boolean
  (context (:pointer g-main-context)))

(defcfun (g-main-context-wait "g_main_context_wait" :library glib) :boolean
  (context (:pointer g-main-context))
  (cond (:pointer g-cond))
  (mutex (:pointer g-mutex)))

(defcfun (g_main_context_prepare "g_main_context_prepare" :library glib) :boolean
  (context (:pointer g-main-context))
  (priority-ret (:pointer :int)))

(defcfun (g_main_context_query "g_main_context_query" :library glib) :int
  (context (:pointer g-main-context))
  (max-priority :int)
  (timeout-ret (:pointer :int))
  (fds-ret (:pointer g-poll-fd))
  (n-dfs :int))

(defcfun (g-main-context-check "g_main_context_check" :library glib) :int
  (context (:pointer g-main-context))
  (max-priority :int)
  (fds (:pointer g-poll-fd))
  (n-fds :int))

(defcfun (g-main-context-dispatch "g_main_context_dispatch" :library glib) :void
  (context (:pointer g-main-context)))

(defcfun (g-main-context-set-poll-func "g_main_context_set_poll_func" :library glib) :void
  (context (:pointer g-main-context))
  (func :pointer))

(defcfun (g-main-context-get-poll-func "g_main_context_get_poll_func" :library glib) :pointer
  (context (:pointer g-main-context)))

(defcfun (g-main-context-add-poll "g_main_context_add_poll" :library glib) :void
  (context (:pointer g-main-context))
  (fd (:pointer g-poll-fd))
  (priority :int))

(defcfun (g-main-context-remove-poll "g_main_context_remove_poll" :library glib) :void
  (context (:pointer g-main-context))
  (fd (:pointer g-poll-fd)))

(defcfun (g-main-depth "g_main_depth" :library glib) :int)

(defcfun (g-main-current-source "g_main_current_source" :library glib) (:pointer g-source))

(defcfun (g-timeout-source-new "g_timeout_source_new" :library glib) (:pointer g-source)
  (interval-milliseconds :int))

(defcfun (g-timeout-source-new-seconds "g_timeout_source_new_seconds" :library glib) (:pointer g-source)
  (interval-seconds :int))

(defcfun (g-timeout-add "g_timeout_add" :library glib) :uint
  (interval-milliseconds :uint)
  (function :pointer)
  (data :pointer))

(defcfun (g-timeout-add-full "g_timeout_add_full" :library glib) :uint
  (priority :int)
  (interval-milliseconds :uint)
  (function :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defcfun (g-timeout-add-seconds "g_timeout_add_seconds" :library glib) :uint
  (interval-seconds :uint)
  (function :pointer)
  (data :pointer))

(defcfun (g-timeout-add-seconds-full "g_timeout_add_seconds_full" :library glib) :uint
  (priority :int)
  (interval-seconds :uint)
  (function :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defcfun (g-idle-source-new "g_idle_source_new" :library glib) (:pointer g-source))

(defcfun (g-idle-add "g_idle_add" :library glib) :uint
  (function :pointer)
  (data :pointer))

(defcfun (g-idle-add-full "g_idle_add_full" :library glib) :uint
  (priority :uint)
  (function :pointer)
  (data :pointer)
  (notify :pointer))

(defcfun (g-idle-remove-by-data "g_idle_remove_by_data" :library glib) :boolean
  (data :pointer))

;(defctype g-pid :int) ;;TODO: might work on amd64 linux, but on others

;; Omitted GPid, g_child_add_watch, g_child_add_watch_full

(defcfun (g-source-new "g_source_new" :library glib) (:pointer g-source)
  (source-funcs (:pointer g-source-funcs))
  (struct-size :uint))

(defcfun (g-source-ref "g_source_ref" :library glib) (:pointer g-source)
  (source (:pointer g-source)))

(defcfun (g-source-unref "g_source_unref" :library glib) :void
  (source (:pointer g-source)))

(defcfun (g-source-set-funcs "g_source_set_funcs" :library glib) :void
  (source (:pointer g-source))
  (funcs (:pointer g-source-funcs)))

(defcfun (g-source-attach "g_source_attach" :library glib) :uint
  (source (:pointer g-source))
  (context (:pointer g-main-context)))

(defcfun (g-source-destroy "g_source_destroy" :library glib) :void
  (source (:pointer g-source)))

(defcfun (g-source-is-destroyed "g_source_is_destroyed" :library glib) :boolean
  (source (:pointer g-source)))

(defcfun (g-source-set-priority "g_source_set_priority" :library glib) :void
  (source (:pointer g-source))
  (priority :int))

(defcfun (g-source-get-priority "g_source_get_priority" :library glib) :int
  (source (:pointer g-source)))

(defcfun (g-source-set-can-recurse "g_source_set_can_recurse" :library glib) :void
  (source (:pointer g-source))
  (can-recurse :boolean))

(defcfun (g-source-get-can-recurse "g_source_get_can_recurse" :library glib) :boolean
  (source (:pointer g-source)))

(defcfun (g-source-get-id "g_source_get_id" :library glib) :uint
  (source (:pointer g-source)))

(defcfun (g-source-get-context "g_source_get_context" :library glib) (:pointer g-main-context)
  (source (:pointer g-source)))

(defcfun (g-source-set-callback "g_source_set_callback" :library glib) :void
  (source (:pointer g-source))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defcfun (g-source-add-poll "g_source_add_poll" :library glib) :void
  (source (:pointer g-source))
  (fd (:pointer g-poll-fd)))

(defcfun (g-source-remove-poll "g_source_remove_poll" :library glib) :void
  (source (:pointer g-source))
  (fd (:pointer g-poll-fd)))

(defcfun (g-source-get-current-time "g_source_get_current_time" :library glib) :void
  (source (:pointer g-source))
  (timeval-ret (:pointer g-time-val)))

(defcfun (g-source-remove "g_source_remove" :library glib) :boolean
  (id :uint))

(defcfun (g-source-remove-by-funcs-user-data "g_source_remove_by_funcs_user_data" :library glib) :boolean
  (funcs (:pointer g-source-funcs))
  (data :pointer))

(defcfun (g-source-remove-by-user-data "g_source_remove_by_user_data" :library glib) :boolean
  (data :pointer))

;;
;; Core Application Support - Threads
;;

(defcenum g-thread-error
  :g-thread-error-again)

;omitted: struct GThreadFunctions

(defcfun (g-thread-init "g_thread_init") :void
  (vtable :pointer))

(defvar *threads-initialized-p* nil)

(at-init
  (unless *threads-initialized-p*
    (g-thread-init (null-pointer))
    (setf *threads-initialized-p* t)))

(defcenum g-thread-priority
  :g-thread-priority-low
  :g-thread-priority-normal
  :g-thread-priority-hight
  :g-thread-priority-urgent)

;omitted: g_thread_create, g_thread_create_full, g_thread_yield, g_thread_exit, g_thread_foreach

(defcfun (g-thread-self "g_thread_self" :library glib) (:pointer g-thread))

(defcfun (g-thread-join "g_thread_join" :library glib) :pointer
  (thread (:pointer g-thread)))

(defcfun (g-thread-priority "g_thread_set_priority" :library glib) :void
  (thread (:pointer g-thread))
  (priority g-thread-priority))

;;;; TODO: Commented g_mutex_*, g_cond* because they are not functions, but called through dispatch table

;; (defcfun (g-mutex-new "g_mutex_new" :library glib) (:pointer g-mutex))

;; (defcfun (g-mutex-lock "g_mutex_lock" :library glib) :void
;;   (mutex (:pointer g-mutex)))

;; (defcfun (g-mutex-try-lock "g_mutex_trylock" :library glib) :boolean
;;   (mutex (:pointer g-mutex)))

;; (defcfun (g-mutex-free "g_mutex_free" :library glib) :void
;;   (mutex (:pointer g-mutex)))

;omitted: GStaticMutex, GStaticRWLock stuff

;; (defcfun (g-cond-new "g_cond_new" :library glib) (:pointer g-cond))

;; (defcfun (g-cond-signal "g_cond_signal" :library glib) :void
;;   (cond (:pointer g-cond)))

;; (defcfun (g-cond-broadcast "g_cond_broadcast" :library glib) :void
;;   (cond (:pointer g-cond)))

;; (defcfun (g-cond-wait "g_cond_wait" :library glib) :void
;;   (cond (:pointer g-cond))
;;   (mutex (:pointer g-mutex)))

;; (defcfun (g-cond-timed-wait "g_cond_timed_wait" :library glib) :boolean
;;   (cond (:pointer g-cond))
;;   (mutex (:pointer g-mutex))
;;   (abs-time (:pointer g-time-val)))

;; (defcfun (g-cond-free "g_cond_free" :library glib) :void
;;   (cond (:pointer g-cond)))

;omitted: GPrivate, GOnce stuff

;omitted: Thread pools, Asynchronous queues, Dynamic Loading of Modules,
; Memory Allocation, IO Channels, Error Reporting, Message Output and Debugging  Functions, Message Logging

(defcfun g-free :void
  (ptr :pointer))

(defcfun (g-malloc "g_malloc0") :pointer
  (n-bytes gsize))

(defcfun g-strdup :pointer
  (str (:string :free-to-foreign t)))

;omitted all GLib Utilites
;TODO: omitted Date and Time Functions


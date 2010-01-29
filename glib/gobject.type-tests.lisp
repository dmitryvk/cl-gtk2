(defpackage #:gtype-tests
  (:use #:cl #:iter #:gobject #:gobject.ffi #:5am)
  (:export #:run-all-tests)
  (:import-from #:gobject.ffi #:%gtype #:gtype #:gtype-name #:gtype-%id #:gtype-id #:invalidate-gtypes))

(in-package #:gtype-tests)

(def-suite gtype)

(in-suite gtype)

(defun run-all-tests ()
  (run! 'gtype))

;; Normal things

(test normal.1
  (finishes (%gtype "gint"))
  (finishes (%gtype "glong"))
  (finishes (%gtype +g-type-pointer+)))

(test normal.eq
  (is (eq (%gtype "gint") (%gtype "gint")))
  (is (eq (%gtype "GObject") (%gtype "GObject")))
  (is (not (eq (%gtype "gint") (%gtype "GObject"))))
  (is (eq (%gtype "gchararray") (%gtype +g-type-string+))))

(test normal.boundary
  (is (null (%gtype 0)))
  (is (null (%gtype nil)))
  (signals warning (%gtype "foobarbaz"))
  (signals error (%gtype 1)))

(test normal.trans
  (is (string= (gtype-name (%gtype "gint")) "gint"))
  (is (eql (gtype-id (%gtype "gint")) +g-type-int+)))

;; Clear mappings

(test clear.simple
  (let ((type (%gtype "gint")))
    (is (eql (gtype-id type) +g-type-int+))
    (invalidate-gtypes)
    (is (null (gtype-%id type)))
    (is (eql (gtype-id type) +g-type-int+))
    (invalidate-gtypes)
    (is (eq type (%gtype "gint")))
    (invalidate-gtypes)
    (is (eq type (%gtype +g-type-int+)))))

(test clear.1
  (let ((type (%gtype "gint")))
    (invalidate-gtypes)
    (is (null (gtype-%id type)))
    (%gtype +g-type-int+)
    (is (not (null (gethash +g-type-int+ gobject.ffi::*id-to-gtype*))))
    (is (not (null (gtype-%id type))))))

;; Core saving

(defvar *gi* (%gtype +g-type-int+))

(test core.saving
  (is (eq *gi* (%gtype +g-type-int+)))
  (is (eq (gtype +g-type-int+) (%gtype +g-type-int+))))

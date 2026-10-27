(defpackage #:cl-colorist/tests
  (:use #:cl)
  (:import-from #:cl-colorist
                #:*color-level*
                #:ansi-control-end
                #:basic-color
                #:basic-color-names
                #:color-fallback
                #:color-indexed-fallback
                #:color-kind
                #:color-type-error
                #:color-value
                #:colorize
                #:effective-color-level
                #:hex-color
                #:indexed-color
                #:make-style
                #:paint
                #:reset-sequence
                #:rgb-color
                #:sgr-sequence
                #:strip-ansi
                #:style-conflict-error
                #:style-conflict-error-attributes
                #:terminal-color-level
                #:text-index-error
                #:visible-length)
  (:export #:run-tests)
  (:documentation "Tests for cl-colorist, run through RUN-TESTS."))

(in-package #:cl-colorist/tests)

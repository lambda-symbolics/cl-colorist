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
                #:terminal-color-level
                #:visible-length)
  (:export #:run-tests))

(in-package #:cl-colorist/tests)

;;;; 2007-10-30 21:44:35

(defpackage #:com.offbytwo.pathlib-asd
  (:use :cl :asdf))

(in-package :com.offbytwo.pathlib-asd)

(defsystem com.offbytwo.pathlib
  :name "com.offbytwo.pathlib"
  :version "0.1"
  :components ((:file "defpackage")
               (:file "main" :depends-on ("defpackage")))
  :depends-on ())

(defpackage #:com.offbytwo.binlib-asd
  (:use :cl :asdf))

(in-package :com.offbytwo.binlib-asd)

(defsystem com.offbytwo.binlib
  :name "com.offbytwo.binlib"
  :version "0.1"
  :components ((:file "defpackage")
               (:file "main" :depends-on ("defpackage")))
  :depends-on ())

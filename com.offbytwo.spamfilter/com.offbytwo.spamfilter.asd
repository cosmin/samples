;;;; 2007-10-29 22:48:17

(defpackage #:com.offbytwo.spamfilter-asd
  (:use :cl :asdf))

(in-package :com.offbytwo.spamfilter-asd)

(defsystem com.offbytwo.spamfilter
  :name "com.offbytwo.spamfilter"
  :version "0.1"
  :components ((:file "defpackage")
               (:file "main" :depends-on ("defpackage")))
  :depends-on ())

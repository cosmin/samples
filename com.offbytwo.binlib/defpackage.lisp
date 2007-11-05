(in-package :common-lisp-user)

(defpackage :com.offbytwo.binlib
  (:nicknames :binlib)
  (:use :cl)
  (:export :define-binary-class
	   :define-tagged-binary-class
	   :define-binary-type
	   :read-value
	   :write-value
	   :*in-progress-objects*
	   :parent-of-type
	   :current-binary-object
	   :unsigned-integer
	   :u1
	   :u2
	   :u3
	   :u4
	   :generic-string
	   :generic-terminated-string
	   :iso-8859-1-char
	   :iso-8859-1-string
	   :iso-8859-1-terminated-string
	   :ucs-2-char
	   :swap-bytes
	   :ucs-2-char-type
	   :ucs-2-string
	   :ucs-2-terminated-string
	   :+null+
   ))
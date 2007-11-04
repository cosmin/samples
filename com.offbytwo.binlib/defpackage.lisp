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
	   :+null+
   ))
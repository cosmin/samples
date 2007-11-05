(in-package :com.offbytwo.id3v2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; read the ID3 from the given file
(defun read-id3 (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'id3-tag in)))

;; print ID3 tag information about the given file
(defun show-tag-header (file)
  (with-slots (identifier major-version revision flags size) (read-id3-file)
    (format t "~a ~d.~d ~8,'0b ~d bytes -- ~a~%"
	    identifier major-version revision flags size (enough-namestring file))))

;; check if the file is mp3 (by checking extension)
(defun mp3-p (file)
  (and
   (not (directory-pathname-p file))
   (string-equal "mp3" (pathname-type file))))

;; walk the directory and print id3 info for all the mp3 files
(defun show-tag-headers (dir)
  (walk-directory dir #'show-tag-header :test #'mp3-p))

;; count how many of each ID3 version we have
(defun count-versions (dir)
  (let ((versions (mapcar #'(lambda (x) (cons x 0)) '(2 3 4))))
    (flet ((count-version (file)
	     (incf (cdr (assoc (major-version (read-id3 file)) versions)))))
      (walk-directory dir #'count-version :test #'mp3-p))
    versions))

;; check if the file has a valid ID3 section
(defun id3-p (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (string= "ID3" (read-value 'iso-8859-1-string in :length 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; condition to be raised if we're in padding
(define-condition in-padding () ())

;; method will specialize on the type of frame and return the header length
(defgeneric frame-header-size (frame))

(defmethod frame-header-size ((frame id3v2.2-frame)) 6)

(defmethod frame-header-size ((frame id3v2.3-frame)) 10)

;;; helper functions to read data from the header flags section
(defun frame-compressed-p (flags) (logbit 7 flags))

(defun frame-encrypted-p (flags) (logbit 6 flags))

(defun frame-grouped-p (flags) (logbit 5 flags))

(defun extended-p (flags) (logbitp 6 flags))

(defun crc-p (flags extra-flags)
  (and (extended-p flags) (logbitp 15 extra-flags)))

;; type used to hold raw byte data 
(define-binary-type raw-bytes (size)
  (:reader (in)
	   (let ((buf (make-array size :element-type '(unsigned-byte 8))))
	     (read-sequence buf in)
	     buf))
  (:writer (out buf)
	   (write-sequence buf out)))

(define-binary-type optional (type if)
  (:reader (in)
	   (when if (read-value type in)))
  (:writer (out value)
	   (when if (write-value type out value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; base class for id3-frames, use find-frame-class to determine which version to use
(define-tagged-binary-class id3-frame ()
  ((id (frame-id :length 3))
   (size u3))
  (:dispatch (find-frame-class id)))

(defun find-frame-class (id)
  (ecase (length id)
    (3 'generic-frame-v2.2)
    (4 'generic-frame-v2.3)))


(define-binary-class generic-frame()
  ((data (raw-bytes :size (data-bytes (current-binary-object))))))

(define-tagged-binary-class id3v2.2-frame ()
  ((id (frame-id :length 3))
   (size u3))
  (:dispatch (find-frame-class id)))

(define-tagged-binary-class id3v2.3-frame ()
  ((id                 (frame-id :length 4))
   (size               u4)
   (flags              u2)
   (decompressed-size  (optional :type 'u4 :if (frame-compressed-p flags)))
   (encryption-scheme  (optional :type 'u1 :if (frame-encrypted-p flags)))
   (grouping-identity  (optional :type 'u1 :if (frame-grouped-p flags))))
  (:dispatch (find-frame-class id)))


(defgeneric data-bytes (frame))

(defmethod data-bytes ((frame id3v2.2-frame))
  (size frame))

(defmethod data-bytes ((frame id3v2.3-frame))
  (let ((flags (flags frame)))
    (- (size frame)
       (if (frame-compressed-p flags) 4 0)
       (if (frame-encrypted-p flags) 1 0)
       (if (frame-grouped-p flags) 1 0))))

(define-binary-class generic-frame-v2.2 (id3v2.2-frame generic-frame) ())

(define-binary-class generic-frame-v2.3 (id3v2.3-frame generic-frame) ())



(define-binary-type id3-frames (tag-size frame-type)
  (:reader (in)
	   (loop with to-read = tag-size
	      while (plusp to-read)
	      for frame = (read-frame frame-type in)
	      while frame
	      do (decf to-read (+ (frame-header-size frame) (size frame)))
	      collect frame
	      finally (loop repeat (1- to-read) do (read-byte in))))
  (:writer (out frames)
	   (loop with to-write = tag-size
	      for frame in frames
	      do (write-value 'id3-frame out frame)
	      (decf to-write (+ (frame-header-size frame) (size frame)))
	      finally (loop repeat to-write do (write-byte 0 out)))))
  
(define-tagged-binary-class id3-tag ()
  ((identifier      (iso-8859-1-string :length 3))
   (major-version   u1)
   (revision        u1)
   (flags           u1))
  (:dispatch
   (ecase major-version
     (2 'id3v2.2-tag)
     (3 'id3v2.3-tag))))

(define-binary-class id3v2.2-tag (id3-tag)
  ((frames (id3-frames :tag-size size :frame-type 'id3v2.2-frame))))

(define-binary-class id3-v2.3-tag (id3-tag)
  ((extended-header-size (optional :type 'u4 :if (extended-p flags)))
   (extra-flags          (optional :type 'u2 :if (extended-p flags)))
   (padding-size         (optional :type 'u4 :if (extended-p flags)))
   (crc                  (optional :type 'u4 :if (crc-p flags extra-flags)))
   (frames               (id3-frames :tag-size size :frame-type 'id3v2.3-frame))))


(define-binary-type frame-id (length)
  (:reader (in)
	   (let ((first-byte (read-byte in)))
	     (when (= first-byte 0) (signal 'in-padding))
	     (let ((rest (read-value 'iso-8859-1-string in :length (1- length))))
	       (concatenate
		'string (string (code-char first-byte)) rest))))
  (:writer (out id)
	   (write-value 'iso-8859-1-string out id :length length)))




(defun read-frame (frame-type in)
  (handler-case (read-value frame-type in)
    (in-padding () nil)))


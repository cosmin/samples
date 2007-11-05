(in-package :com.offbytwo.id3v2)

(define-binary-class id3-tag ()
  ((identifier      (iso-8859-1-string :length 3))
   (major-revision  u1)
   (revision        u1)
   (flags           u1)
   (size            id3-tag-size)))

(defun read-id3 (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'id3-tag in)))

(defun show-tag-header (file)
  (with-slots (identifier major-version revision flags size) (read-id3-file)
    (format t "~a ~d.~d ~8,'0b ~d bytes -- ~a~%"
	    identifier major-version revision flags size (enough-namestring file))))

(defun mp3-p (file)
  (and
   (not (directory-pathname-p file))
   (string-equal "mp3" (pathname-type file))))

(defun show-tag-headers (dir)
  (walk-directory dir #'show-tag-header :test #'mp3-p))

(defun count-versions (dir)
  (let ((versions (mapcar #'(lambda (x) (cons x 0)) '(2 3 4))))
    (flet ((count-version (file)
	     (incf (cdr (assoc (major-version (read-id3 file)) versions)))))
      (walk-directory dir #'count-version :test #'mp3-p))
    versions))

(defun id3-p (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (string= ("ID3" (read-value 'iso-8859-1-string in :length 3)))))

(define-tagged-binary-class id3-frame ()
  ((id (iso-8859-1-string :length 3))
   (size u2))
  (:dispatch (find-frame-class id)))

(define-binary-class generic-frame (id3-frame)
  ((data (raw-bytes :size size))))

(define-binary-type raw-bytes (size)
  (:reader (in)
	   (let ((buf (make-array size :element-type '(unsigned-byte 8))))
	     (read-sequence buf in)
	     buf))
  (:writer (out buf)
	   (write-sequence buf out)))

(defun find-frame-class (id)
  (declare (ignore id))
  'generic-frame)

(define-binary-type id3-frames (tag-size)
  (:reader (in)
	   (loop with to-read = tag-size
	      while (plusp to-read)
	      for frame = (read-frame in)
	      while frame
	      do (decf to-read (+ 6 (size frame)))
	      collect frame
	      finally (loop repeat (1- to-read) do (read-byte in))))
  (:writer (out frames)
	   (loop with to-write = tag-size
	      for frame in frames
	      do (write-value 'id3-frame out frame)
	      (decf to-write (+ 6 (size frame)))
	      finally (loop repeat to-write do (write-byte 0 out)))))
  
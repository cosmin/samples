(in-package :com.offbytwo.binlib)

(define-binary-type unsigned-integer (bytes)
  (:reader (in)
    (loop with value = 0
       for low-bit downfrom  (* 8 (1- bytes)) to 0 by 8 do
	 (setf (ldb (byte 8 low-bit) value) (read-byte in))
	finally (return value)))
  (:writer (out value)
    (loop for low-bit downfrom (* 8 (1- bytes)) to 0 by 8
	 do (write-byte (ldb (byte 8 low-bit) value) out))))

(define-binary-type u1 () (unsigned-integer :bytes 1))
(define-binary-type u2 () (unsigned-integer :bytes 2))
(define-binary-type u3 () (unsigned-integer :bytes 3))
(define-binary-type u4 () (unsigned-integer :bytes 4))

(define-binary-type generic-string (length character-type)
  (:reader (in)
	   (let ((string (make-string length)))
	     (dotimes (i length)
	       (setf (char string i) (read-value character-type in)))
	     string))
  (:writer (out string)
	   (dotimes (i length)
	     (write-value character-type out (char string i)))))

(define-binary-type generic-terminated-string (terminator character-type)
  (:reader (in)
    (with-output-to-string (s)
      (loop for char = (read-value character-type in)
	 until (char= char terminator) do (write-char char s))))
  (:writer (out string)
    (loop for char across string
       do (write-value character-type out char)
       finally (write-value character-type out terminator))))

(define-binary-type iso-8859-1-char ()
  (:reader (in)
    (let ((code (read-byte in)))
      (or (code-char code) (error "Character code ~d not supported" code))))
  (:writer (out char)
    (let ((code (char-code char)))
      (if (<= 0 code #xff)
	  (write-byte code out)
	  (error "Illegal character for iso-8859-1 encoding: character: ~c with code: ~d" char code)))))

(define-binary-type iso-8859-1-string (length)
  (generic-string :length length :character-type 'iso-8859-1-char))

(define-binary-type iso-8859-1-terminated-string (terminator)
  (generic-terminated-string :terminator terminator
			     :character-type 'iso-8859-1-char))

(define-binary-type ucs-2-char (swap)
  (:reader (in)
	   (let ((code (read-value 'u2 in)))
	     (when swap (setf code (swap-bytes code)))
	     (or (code-char code) (error "Character code ~d not supported" code))))
  (:writer (out char)
	   (let ((code (char-code char)))
	     (unless (<= 0 code #xffff)
	       (error "Illegal character for ucs-2 encoding: ~c with char-code: ~d" char code))
	     (when swap (setf code (swap-bytes code)))
	     (write-value 'u2 out code))))

(defun swap-bytes (code)
  (assert (<= code #xffff))
  (rotatef (ldb (byte 8 0) code) (ldb (byte 8 8) code))
  code)

(define-binary-type ucs-2-char-big-endian () (ucs-2-char :swap nil))
(define-binary-type ucs-2-char-little-endian () (ucs-2-char :swap t))

(defun ucs-2-char-type (byte-order-mark)
  (ecase byte-order-mark
    (#xfeff 'ucs-2-char-big-endian)
    (#xfffe 'ucs-2-char-little-endian)))

(define-binary-type ucs-2-string (length)
  (:reader (in)
	   (let ((byte-order-mark (read-value 'u2 in))
		 (characters (1- (/ length 2))))
	     (read-value
	      'generic-string in
	      :length characters
	      :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
	   (write-value 'u2 out #xfeff)
	   (write-value
	    'generic-string out string
	    :length (length string)
	    :character-type (ucs-2-char-type #xfeff))))

(define-binary-type ucs-2-terminated-string (terminator)
  (:reader (in)
	   (let ((byte-order-mark (read-value 'u2 in)))
	     (read-value
	      'generic-terminated-string in
	      :terminator terminator
	      :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
	   (write-value 'u2 out #xfeff)
	   (write-value
	    'generic-terminated-string out string
	    :terminator terminator
	    :character-type (ucs-2-char-type #xfeff))))
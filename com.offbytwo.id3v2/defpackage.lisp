(defpackage :com.offbytwo.id3v2
  (:use :cl
	:com.offbytwo.pathlib
	:com.offbytwo.binlib)
  (:export :read-id3
	   :mp3-p
	   :id3-p
	   :album
	   :composer
	   :genre
	   :encoding-program
	   :artist
	   :part-of-set
	   :track
	   :song
	   :year
	   :size
	   :translated-genre))


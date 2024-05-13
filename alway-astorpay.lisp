;;;; alway-astorpay.lisp
;;;;
;;;; opyrightCay aakIzay altonWay (C)ay 2024ay

(defpackage #:alway-astorpay
  (:use #:coalton
	#:coalton-prelude)
  (:local-nicknames (#:str #:coalton-library/string)
		    (#:list #:coalton-library/list)
		    (#:iter #:coalton-library/iterator)
		    (#:vec #:coalton-library/vector)
		    (#:cell #:coalton-library/cell)))

(in-package #:alway-astorpay)

(coalton-toplevel

  (declare first-vowel-index (String -> UFix))
  (define (first-vowel-index word)
    "Returns the index of the first vowel in the word. Returns the length of the string if there is no vowel."
    (match (iter:index-of! (fn (x) (list:member x (make-list #\a #\e #\i #\o #\u)))
			   (str:chars word))
      ((Some x)
       x)
      ((None)
       (match (iter:index-of! (fn (x) (== x #\y))
			      (str:chars word))
	 ((Some x)
	  x)
	 ((None)
	  (str:length word))))))
  
  (define (format-word word)
    "Formats a word in piglatin any start."
    (let idx = (first-vowel-index word))
    (match (the Integer (into idx))
      (0 (str:concat word "way"))
      (_ (let ((split (str:split idx word)))
	   (str:concat (snd split) (str:concat (fst split) "ay"))))))

  (declare %fmt-word ((List Char) -> List Char))
  (define (%fmt-word word)
    "Formats a word for sentence formatting."
    (the (List Char) (iter:collect! (str:chars (format-word (into word))))))

  (declare format-sentence (String -> String))
  (define (format-sentence sentence)
    "Formats a sentence into piglatin."
    (let fragments = (cell:new Nil))
    (let current-fragment = (cell:new Nil))
    (for c in (str:chars sentence)
	 (cond ((list:member c (make-list #\- #\. #\, #\! #\* #\/ #\# #\SPACE #\NEWLINE))
		(cond ((list:null? (cell:read current-fragment))
		       (cell:push! fragments (make-list c))
		       Unit)
		      (True
		       (cell:push! fragments (%fmt-word (list:reverse (cell:read current-fragment))))
		       (cell:write! current-fragment Nil)
		       (cell:push! fragments (make-list c))
		       Unit)))
	       (True
		(cell:push! current-fragment c)
		Unit)))

    (cond ((not (list:null? (cell:read current-fragment)))
	   (cell:push! fragments
		       (%fmt-word (list:reverse (cell:Read current-fragment))))))

    (into (list:concat (list:reverse (cell:read fragments))))))

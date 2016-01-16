;; Copyright (c) 2012, Miron Brezuleanu
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)
(defpackage inmemory-cache.messagepack-test
  (:use :cl
        :prove
        :inmemory-cache.util
        :inmemory-cache.messagepack))
(in-package inmemory-cache.messagepack-test)



(plan nil)

(diag "Test encoding of integers.")
(is (encode 0) #(0) :test #'equalp)
(is (encode 127) #(127) :test #'equalp)
(is (encode 128) #(#xCC 128) :test #'equalp)
(is (encode -1) #(255) :test #'equalp)
(is (encode -32) #(#xE0) :test #'equalp)
(is (encode -33) #(#xD0 #xDF) :test #'equalp)
(is (encode 256) #(#xCD #x01 #x00) :test #'equalp)
(is (encode 65535) #(#xCD #xFF #xFF) :test #'equalp)
(is (encode 65536) #(#xCE #x00 #x01 #x00 #x00) :test #'equalp)
(is (encode (1- (expt 2 32))) #(#xCE #xFF #xFF #xFF #xFF) :test #'equalp)
(is (encode (1- (expt 2 64))) #(#xCF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF) :test #'equalp)
(is (encode -128) #(#xD0 #x80) :test #'equalp)
(is (encode (- (expt 2 15))) #(#xD1 #x80 #x00) :test #'equalp)
(is (encode (- (expt 2 31))) #(#xD2 #x80 #x00 #x00 #x00) :test #'equalp)
(is (encode (- (expt 2 63))) #(#xD3 #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00) :test #'equalp)

(diag "Test encoding of NIL and T.")
(is (encode nil) #(#xc0) :test #'equalp)
(is (encode t) #(#xc3) :test #'equalp)


(diag "Test encoding of single and double precision floating point numbers.")
#+sbcl
(is (encode 1.0s0) #(#xCA #x3F #x80 #x00 #x00) :test #'equalp)
(is (encode 1.0d0) #(#xCB #x3F #xF0 #x00 #x00 #x00 #x00 #x00 #x00) :test #'equalp)

(diag "Test encoding of strings (raw bytes in msgpack parlance, strings
since I am interested in using msgpack as a binary JSON). Use strings
of various lengths to make sure string length is encoded properly.")
(is (encode "Test string")
    #(#xAB #x54 #x65 #x73 #x74 #x20 #x73 #x74 #x72 #x69 #x6E #x67) :test #'equalp)
(is (encode (with-output-to-string (str)
              (loop repeat 10 do (princ "string" str))))
    #(#xD9 #x3C #x73 #x74 #x72 #x69 #x6E #x67 #x73 #x74 #x72 #x69 #x6E #x67 #x73
      #x74 #x72 #x69 #x6E #x67 #x73 #x74 #x72 #x69 #x6E #x67 #x73 #x74 #x72 #x69 #x6E
      #x67 #x73 #x74 #x72 #x69 #x6E #x67 #x73 #x74 #x72 #x69 #x6E #x67 #x73 #x74 #x72
      #x69 #x6E #x67 #x73 #x74 #x72 #x69 #x6E #x67 #x73 #x74 #x72 #x69 #x6E #x67) :test #'equalp)
(is (subseq  (encode (let ((str (make-string 360000)))
                       (loop for i from 1 to 60000
                          do (setf (subseq str (* 6 (1- i)) (* 6 i)) "string"))
                       str))
             0 15)
    #(#xDB #x00 #x05 #x7E #x40 #x73 #x74 #x72 #x69 #x6E #x67 #x73 #x74 #x72 #x69) :test #'equalp)

(diag "Test encoding of arrays of various sizes. Make sure that arrays
encode properly.")
(is (encode (coerce '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) '(vector)))
    #(#x9F #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x0C #x0D #x0E #x0F) :test #'equalp)
(is (encode (coerce (loop for i from 1 to 16 collect i) '(vector)))  
    #(#xDC #x00 #x10 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x0C #x0D
      #x0E #x0F #x10) :test #'equalp)
(is (subseq (encode (coerce (loop for i from 1 to 65536 collect i) '(vector)))
            0
            15)
    #(#xDD #x00 #x01 #x00 #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A) :test #'equalp)

(diag "Test maps of various sizes, make sure that lengths are encoded
  properly.")
(labels ((make-map (size)
           (let ((result (make-hash-table)))
             (loop
                for i from 1 to size
                do (setf (gethash i result) (- i)))
             result)))
  (is (subseq (encode (make-map 10)) 0 1)
      #(#x8A) :test #'equalp)
  (is (subseq  (encode (make-map 16)) 0 3)
      #(#xDE #x00 #x10) :test #'equalp)
  (is (subseq (encode (make-map 65536)) 0 5)
      #(#xDF #x00 #x01 #x00 #x00) :test #'equalp))



(diag "Test that (equalp (decode (encode data)) data) for integers (that
  can be encoded as Message Pack integers.")
(is (decode-from-buffer (encode 100)) 100 :test #'=)
(is (decode-from-buffer (encode -30)) -30 :test #'=)
(is (decode-from-buffer (encode 12345)) 12345 :test #'=)
(is (decode-from-buffer (encode 2390493)) 2390493 :test #'=)
(is (decode-from-buffer (encode 2390493000)) 2390493000 :test #'=)

(diag "Test that (equalp (decode (encode data)) data) for bools.")
(is (decode-from-buffer (encode t)) t :test #'eql)
(is (decode-from-buffer (encode nil)) nil :test #'eql)

(diag "Test that (equalp (decode (encode data)) data) for floats.")
#+ (or sbcl ccl) (is (decode-from-buffer (encode 100d0)) 100d0 :test #'=)
#+ sbcl (is (decode-from-buffer (encode 102s0)) 102s0 :test #'=)

(diag "Test that (equalp (decode (encode data)) data) holds for strings.")
(let ((*print-pretty* nil))
  (let ((short-string "test")
        (medium-string (with-output-to-string (str)
                         (loop repeat 10 do (princ "test" str))))
        (long-string (with-output-to-string (str)
                       (loop repeat (expt 10 5) do (princ "test" str)))))
    (is (decode-from-buffer (encode short-string)) short-string :test #'string=)
    (is (decode-from-buffer (encode medium-string)) medium-string :test #'string=)
    (is (decode-from-buffer (encode long-string)) long-string :test #'string=)))

(diag "Test that (equalp (decode (encode data)) data) holds for arrays.")
(let ((short-array #(1 2 3))
      (medium-array (make-array 1000 :initial-element 10))
      (long-array (make-array 70000 :initial-element 10)))
  (is (decode-from-buffer (encode short-array)) short-array :test #'equalp)
  (is (decode-from-buffer (encode medium-array)) medium-array :test #'equalp)
  (is (decode-from-buffer (encode long-array)) long-array :test #'equalp))


(defun make-map (size)
  (let ((result (make-hash-table)))
    (loop
       for i from 1 to size
       do (setf (gethash i result) (- i)))
    result))

(diag "Test that (equalp (decode (encode data)) data) holds for hash
tables that have #'equalp as test.")
(let ((small-map (make-map 10))
      (medium-map (make-map 1000))
      (big-map (make-map 70000)))
  (is (decode-from-buffer (encode small-map)) small-map :test #'equalp)
  (is (decode-from-buffer (encode medium-map)) medium-map :test #'equalp)
  (is (decode-from-buffer (encode big-map)) big-map :test #'equalp))

(finalize)

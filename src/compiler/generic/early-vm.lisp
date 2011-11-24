;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; the number of bits at the low end of a pointer used for type
;;; information
(def!constant n-lowtag-bits
  (integer-length (1- (/ (* 2 n-word-bits) n-byte-bits))))
;;; a mask to extract the low tag bits from a pointer
(def!constant lowtag-mask (1- (ash 1 n-lowtag-bits)))
;;; the exclusive upper bound on the value of the low tag bits from a
;;; pointer
(def!constant lowtag-limit (ash 1 n-lowtag-bits))
;;; the number of tag bits used for a fixnum
(def!constant n-fixnum-tag-bits
    (if (= 64 sb!vm:n-word-bits)
        ;; On 64-bit targets, this may be as low as 1 (for 63-bit
        ;; fixnums) and as high as 3 (for 61-bit fixnums).  The
        ;; constraint on the low end is that we need at least one bit
        ;; to determine if a value is a fixnum or not, and the
        ;; constraint on the high end is that it must not exceed
        ;; WORD-SHIFT (defined below) due to the use of unboxed
        ;; word-aligned byte pointers as boxed values in various
        ;; places.  FIXME: This should possibly be exposed for
        ;; configuration via customize-target-features.
        1
        ;; On 32-bit targets, this may be as low as 2 (for 30-bit
        ;; fixnums) and as high as 2 (for 30-bit fixnums).  The
        ;; constraint on the low end is simple overcrowding of the
        ;; lowtag space, and the constraint on the high end is that it
        ;; must not exceed WORD-SHIFT.
        (1- n-lowtag-bits)))
;;; the fixnum tag mask
(def!constant fixnum-tag-mask (1- (ash 1 n-fixnum-tag-bits)))
;;; the bit width of fixnums
(def!constant n-fixnum-bits (- n-word-bits n-fixnum-tag-bits))
;;; the bit width of positive fixnums
(def!constant n-positive-fixnum-bits (1- n-fixnum-bits))

;;; the number of bits to shift between word addresses and byte addresses
(def!constant word-shift (1- (integer-length (/ n-word-bits n-byte-bits))))

;;; the number of bytes in a word
(def!constant n-word-bytes (/ n-word-bits n-byte-bits))

;;; the number of bits used in the header word of a data block to store
;;; the type
(def!constant n-widetag-bits 8)
;;; a mask to extract the type from a data block header word
(def!constant widetag-mask (1- (ash 1 n-widetag-bits)))

(def!constant sb!xc:most-positive-fixnum
    (1- (ash 1 n-positive-fixnum-bits))
  #!+sb-doc
  "the fixnum closest in value to positive infinity")
(def!constant sb!xc:most-negative-fixnum
    (ash -1 n-positive-fixnum-bits)
  #!+sb-doc
  "the fixnum closest in value to negative infinity")

(def!constant most-positive-word (1- (expt 2 n-word-bits))
  "The most positive integer that is of type SB-EXT:WORD.")

(def!constant most-positive-exactly-single-float-fixnum
  (min #xffffff sb!xc:most-positive-fixnum))
(def!constant most-negative-exactly-single-float-fixnum
  (max #x-ffffff sb!xc:most-negative-fixnum))
(def!constant most-positive-exactly-double-float-fixnum
  (min #x1fffffffffffff sb!xc:most-positive-fixnum))
(def!constant most-negative-exactly-double-float-fixnum
  (max #x-1fffffffffffff sb!xc:most-negative-fixnum))

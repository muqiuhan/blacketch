;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; block.rkt : Defines how a block works                                          ;;
;;                                                                                ;;
;; Copyright (c) 2022 Muqiu Han                                                   ;;
;;                                                                                ;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy   ;;
;; of this software and associated documentation files (the "Software"), to deal  ;;
;; in the Software without restriction, including without limitation the rights   ;;
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      ;;
;; copies of the Software, and to permit persons to whom the Software is          ;;
;; furnished to do so, subject to the following conditions:                       ;;
;;                                                                                ;;
;; The above copyright notice and this permission notice shall be included in all ;;
;; copies or substantial portions of the Software.                                ;;
;;                                                                                ;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     ;;
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       ;;
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    ;;
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         ;;
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  ;;
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  ;;
;; SOFTWARE.                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/base

(require (only-in openssl/sha1 hex-string->bytes))
(require (only-in sha sha256))
(require (only-in sha bytes->hex-string))
(require racket/serialize)

(define difficulty 2)
(define target (bytes->hex-string
                (make-bytes difficulty 32)))

;; A block should contain the current hash, the previous hash, the data and the timestamp that it was generate
;; Using a hashing algorithm will allow us to confirm that the block is valid.
;; In general, blocks can contain any data, not just transactions, but I'm limiting them to transactions for now.
(struct block
  (current-hash previous-hash data timestamp transaction nonce)
  #:prefab)

;; The block also contains a transaction that is roughly of the following form:
(struct transaction
  (signature from to value)
  #:prefab)

;; Calculates a block's hash with the SHA hashing algorithm.
(define (block/calculate-hash previous-hash timestamp transaction nonce)
  (bytes->hex-string
   (sha256
    (bytes-append
     (string->bytes/utf-8 previous-hash)
     (string->bytes/utf-8 (number->string timestamp))
     (string->bytes/utf-8 (format "~a" (serialize transaction)))
     (string->bytes/utf-8 (number->string nonce))))))


;; Verify block, just hash the block's contents again and compare this hash to the one stored in the block.
(define (block/valid? a-block)
  (equal? (block-current-hash a-block)
          (block/calculate-hash
           (block-previous-hash a-block)
           (block-timestamp a-block)
           (block-transaction a-block)
           (block-nonce a-block))))

;; A block will be considered mined if the hash matches the target, given the difficulty
(define (block/mined? block-hash)
  (equal? (subbytes
           (hex-string->bytes block-hash)
           1 difficulty)
          (subbytes
           (hex-string->bytes target)
           1 difficulty)))

;; Hashcrash
;; This procedure keeps increasing the nonce until a block is valid, at which point it is returned.
;; That is, we continuously change the nonce until sha256 produces a hash that matches the target.
;; This defines the foundations of mining (proof of work).
(define (block/make-and-mine previous-hash timestamp transaction nonce)
  (let ([current-hash (block/calculate-hash previous-hash timestamp transaction nonce)])
    (if (block/mined? current-hash)
        (block current-hash previous-hash transaction timestamp nonce)
        (block/make-and-mine previous-hash timestamp transaction (+ nonce 1)))))

;; Help procedure
(define (block/mine transaction previous-hash)
  (block/make-and-mine previous-hash (current-milliseconds) transaction 1))

(provide block/valid?
         block/mined?
         block/mine
         (struct-out block))

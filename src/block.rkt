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
(require (only-in openssl/sha1 bytes->hex-string))
(require racket/serialize)

(define difficulty 3)
(define target (bytes->hex-string (make-bytes difficulty 32)))

(struct block
  (hash previous-hash transaction timestamp nonce)
  #:prefab)

(define (block/calculate-hash previous-hash timestamp transaction nonce)
  (bytes->hex-string
   (sha256
    (bytes-append
     (string->bytes/utf-8 previous-hash)
     (string->bytes/utf-8 (number->string timestamp))
     (string->bytes/utf-8 (format "~a" (serialize transaction)))
     (string->bytes/utf-8 (number->string nonce))))))

(define (block/valid? bl)
  (equal? (block-hash bl)
          (block/calculate-hash (block-previous-hash bl)
                                (block-timestamp bl)
                                (block-transaction bl)
                                (block-nonce bl))))

; A block is mined if
(define (block/mined? hash)
  ; the hash matches the target, given the difficulty
  (equal? (subbytes (hex-string->bytes hash) 1 difficulty)
          (subbytes (hex-string->bytes target) 1 difficulty)))

; Hashcash implementation
(define (block/make-and-mine previous-hash timestamp transaction nonce)
  (let ([hash (block/calculate-hash previous-hash timestamp transaction nonce)])
    (if (block/mined? hash)
        (block hash previous-hash transaction timestamp nonce)
        (block/make-and-mine previous-hash timestamp transaction (+ nonce 1)))))

; Wrapper around make-and-mine-block
(define (block/mine transaction previous-hash)
  (block/make-and-mine
   previous-hash (current-milliseconds) transaction 1))

(provide (struct-out block)
         block/mine
         block/valid?
         block/mined?)

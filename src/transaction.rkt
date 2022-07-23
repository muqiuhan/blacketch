;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transaction-io.rkt : contain procedures for signing and verifying transactions.             ;;
;;                                                                                             ;;
;; Copyright (c) 2022 Muqiu Han                                                                ;;
;;                                                                                             ;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy                ;;
;; of this software and associated documentation files (the "Software"), to deal               ;;
;; in the Software without restriction, including without limitation the rights                ;;
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell                   ;;
;; copies of the Software, and to permit persons to whom the Software is                       ;;
;; furnished to do so, subject to the following conditions:                                    ;;
;;                                                                                             ;;
;; The above copyright notice and this permission notice shall be included in all              ;;
;; copies or substantial portions of the Software.                                             ;;
;;                                                                                             ;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR                  ;;
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,                    ;;
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE                 ;;
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER                      ;;
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,               ;;
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE               ;;
;; SOFTWARE.                                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/base

(require "transaction-io.rkt")
(require "utils.rkt")
(require (only-in file/sha1 hex-string->bytes))
(require "wallet.rkt")
(require crypto)
(require crypto/all)
(require racket/serialize)

;; A transaction contains a signature, sender, receiver, value, and a list of inputs and outputs (transaction-io objects)
(struct transaction
  (signature from to value inputs outputs)
  #:prefab)

;; Makes an empty, unsigned, and unprocessed (no input outputs) transaction.
(define (transaction/make from to value inputs)
  (transaction "" from to value inputs '()))

;; Create a digital signature with SHA algorithm.
;; The private key is used to encrypt the produced hash.
;; The encrypted hash will represent the digital signature.
(define (transaction/sign from to value)
  (let ([prikey (wallet-private-key from)]
        [pubkey (wallet-public-key from)])
    (bytes->hex-string
     (digest/sign
      (datum->pk-key
       (hex-string->bytes prikey)
       'PrivateKeyInfo
       'sha1
       (bytes-append
        (string->bytes/utf-8 (format "~a" (serialize from)))
        (string->bytes/utf-8 (format "~a" (serialize to)))
        (string->bytes/utf-8 (number->string value))))))))

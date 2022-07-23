;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transaction-io.rkt : The basic way of working with transactions                             ;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The transaction structure will consist of a transaction-io structure (transaction input/output). ;;
;; The transaction input will represent the blockchain address from which the money was sent,       ;;
;; and the transaction output will represent the blockchain address to which the money was sent.    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/base

(require (only-in sha sha256))
(require (only-in openssl/sha1 bytes->hex-string))
(require racket/serialize)

;; This structure contains a hash so that we’re able to verify its validity.
;; It also has a value, an owner, and a timestamp.
(struct transaction-io
  (transaction-hash value owner timestamp)
  #:prefab)

;; Creating a hash and rely on serialization
(define (transaction-io/calculate-hash value owner timestamp)
  (bytes->hex-string
   (sha256
    (bytes-append
     (string->bytes/utf-8 (number->string value))
     (string->bytes/utf-8 (format "~a" (serialize owner)))
     (string->bytes/utf-8  (number->string timestamp))))))

;; transaction-io/make is a helper procedure, that will initialize timestamp as well
(define (transaction-io/make value owner)
  (let ([timestamp (current-milliseconds)])
    (transaction-io (transaction-io/calculate-hash value owner timestamp)
                    value
                    owner
                    timestamp)))

;; Verify transaction-io, if transaction-io hash is equal to the hash of the value, owner, and the timestamp
(define (transaction-io/valid? t-in)
  (equal? (transaction-io-transaction-hash t-in)
          (transaction-io/calculate-hash
           (transaction-io-value t-in)
           (transaction-io-owner t-in)
           (transaction-io-timestamp t-in))))

(provide transaction-io/valid?
         transaction-io/calculate-hash
         transaction-io/make)

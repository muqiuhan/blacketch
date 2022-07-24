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
(require (only-in openssl/sha1 hex-string->bytes))
(require "wallet.rkt")
(require crypto)
(require crypto/all)
(require racket/serialize)

(struct transaction
  (signature from to value inputs outputs)
  #:prefab)

; We need to use all crypto factories for converting the key between hex<->pk-key
(use-all-factories!)

(define (transaction/make from to value inputs)
  (transaction "" from to value inputs '()))

; Return digested signature of a transaction data
(define (transaction/sign from to value)
  (let ([privkey (wallet-private-key from)]
        [pubkey (wallet-public-key from)])
    (bytes->hex-string
     (digest/sign
      (datum->pk-key (hex-string->bytes privkey) 'PrivateKeyInfo)
      'sha1
      (bytes-append
       (string->bytes/utf-8 (format "~a" (serialize from)))
       (string->bytes/utf-8 (format "~a" (serialize to)))
       (string->bytes/utf-8 (number->string value)))))))

; Processing transaction procedure
(define (transaction/process t)
  (letrec ([inputs (transaction-inputs t)]
           [outputs (transaction-outputs t)]
           [value (transaction-value t)]
           [inputs-sum (foldr + 0 (map (lambda (i)
                                         (transaction-io-value i))
                                       inputs))]
           [leftover (- inputs-sum value)]
           [new-outputs
            (list
             (transaction-io/make value (transaction-to t))
             (transaction-io/make leftover (transaction-from t)))])
    (transaction
     (transaction/sign (transaction-from t)
                       (transaction-to t)
                       (transaction-value t))
     (transaction-from t)
     (transaction-to t)
     value
     inputs
     (append new-outputs outputs))))

; Checks the signature validity of a transaction
(define (transaction/valid-signature? t)
  (let ([pubkey (wallet-public-key (transaction-from t))])
    (digest/verify
     (datum->pk-key (hex-string->bytes pubkey) 'SubjectPublicKeyInfo)
     'sha1
     (bytes-append
      (string->bytes/utf-8 (format "~a" (serialize (transaction-from t))))
      (string->bytes/utf-8 (format "~a" (serialize (transaction-to t))))
      (string->bytes/utf-8 (number->string (transaction-value t))))
     (hex-string->bytes (transaction-signature t)))))

; A transaction is valid if...
(define (transaction/valid? t)
  (let ([sum-inputs
         (foldr + 0 (map (lambda (t)
                           (transaction-io-value t))
                         (transaction-inputs t)))]
        [sum-outputs
         (foldr + 0 (map (lambda (t)
                           (transaction-io-value t))
                         (transaction-outputs t)))])
    (and
     ; Its signature is valid
     (transaction/valid-signature? t)
     ; All outputs are valid
     (utils/true-for-all? transaction-io/valid? (transaction-outputs t))
     ; The sum of the inputs is gte the sum of the outputs
     (>= sum-inputs sum-outputs))))

(provide (all-from-out "transaction-io.rkt")
         (struct-out transaction)
         transaction/make
         transaction/process
         transaction/valid?)

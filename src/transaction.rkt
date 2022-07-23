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

(use-all-factories!)

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
      (datum->pk-key (hex-string->bytes prikey)
                     'PrivateKeyInfo)
       'sha1
       (bytes-append
        (string->bytes/utf-8 (format "~a" (serialize from)))
        (string->bytes/utf-8 (format "~a" (serialize to)))
        (string->bytes/utf-8 (number->string value)))))))

;; Create transaction outputs that contain the transaction’s value and leftover money
(define (transaction/process trans)
  (letrec ([inputs (transaction-inputs trans)]
           [outputs (transaction-outputs trans)]
           [value (transaction-value trans)]
           [input-sum (foldr + 0 (map (lambda (i) (transaction-io-value i)) inputs))]
           [leftover (- input-sum value)]
           [new-outputs (list
                         (transaction-io/make value (transaction-to trans))
                         (transaction-io/make value (transaction-from trans)))])
    (transaction
     (transaction/sign (transaction-from trans)
                       (transaction-to trans)
                       (transaction-value trans))
     (transaction-from trans)
     (transaction-to trans)
     value
     inputs
     (append new-outputs outputs))))

;; Checks a transaction signature
(define (transaction/valid-signature? trans)
  (let ([pubkey (wallet-public-key (transaction-from trans))])
    (digest/verify
     (datum->pk-key (hex-string->bytes pubkey)
                    'SubjectPublicKeyInfo)
     'sha1
     (bytes-append (string->bytes/utf-8 (format "~a" (serialize (transaction-from trans))))
                   (string->bytes/utf-8 (format "~a" (serialize (transaction-to trans))))
                   (string->bytes/utf-8 (number->string (transaction-value trans))))
     (hex-string->bytes (transaction-signature trans)))))

;; Signature is valid
(define (transaction/valid? trans)
  (let ([sum-inputs
         (foldr + 0 (map (lambda (t-io) (transaction-io-value t-io))
                         (transaction-inputs trans)))]
        [sum-outputs
         (foldr + 0 (map (lambda (t-io) (transaction-io-value t-io))
                         (transaction-outputs trans)))])
    (and
     (transaction/valid-signature? trans)
     (utils/true-for-all? transaction-io/valid? (transaction-outputs trans))
     (>= sum-inputs sum-outputs))))

(provide (all-from-out "transaction-io.rkt")
         (struct-out transaction)
         transaction/make
         transaction/process
         transaction/valid?)

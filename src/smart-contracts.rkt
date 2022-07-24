;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart-contracts.rkt : The smart contract implementation.                       ;;
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

(require "transaction.rkt")
(require racket/match)

(define (smart-contract/valid-transaction? t c)
  (and (smart-contract/eval t c)
       (transaction/valid? t)))

(define (smart-contract/eval t c)
  (match c
    [(? number? x) x]
    [(? string? x) x]
    [`() #t]
    [`true #t]
    [`false #f]
    [`(if ,co ,tr ,fa) (if (smart-contract/eval t co)
                           (smart-contract/eval t tr)
                           (smart-contract/eval t fa))]
    [`(+ ,l ,r) (+ (smart-contract/eval t l) (smart-contract/eval t r))]
    [`from (transaction-from t)]
    [`to (transaction-to t)]
    [`value (transaction-value t)]
    [`(+ ,l ,r) (+ (smart-contract/eval t l) (smart-contract/eval t r))]
    [`(* ,l ,r) (* (smart-contract/eval t l) (smart-contract/eval t r))]
    [`(- ,l ,r) (- (smart-contract/eval t l) (smart-contract/eval t r))]
    [`(= ,l ,r) (= (smart-contract/eval t l) (smart-contract/eval t r))]
    [`(> ,l ,r) (> (smart-contract/eval t l) (smart-contract/eval t r))]
    [`(< ,l ,r) (< (smart-contract/eval t l) (smart-contract/eval t r))]
    [`(and ,l ,r) (and (smart-contract/eval t l) (smart-contract/eval t r))]
    [`(or ,l ,r) (or (smart-contract/eval t l) (smart-contract/eval t r))]
    [else #f]))

(provide smart-contract/valid-transaction?)

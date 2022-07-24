;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main-helper.rkt :                                                                           ;;
;; This file will import everything from blockchain.rkt and utils.rkt and                      ;;
;; will provide some printing procedures                                                       ;;
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

(require "./src/blockchain.rkt")
(require "./src/utils.rkt")
(require "./src/peer-to-peer.rkt")

(define (format-transaction t)
  (format "...~a... sends ...~a... an amount of ~a."
          (substring (wallet-public-key (transaction-from t)) 64 80)
          (substring (wallet-public-key (transaction-to t)) 64 80)
          (transaction-value t)))

(define (print-block bl)
  (let ([block-info (format "| Hash:\t~a\n| Hash_p:\t~a\n| Timestamp:\t~a\n| Nonce:\t~a\n| Data:\t~a\n"
                            (block-hash bl)
                            (block-previous-hash bl)
                            (block-timestamp bl)
                            (block-nonce bl)
                            (format-transaction (block-transaction bl)))])
    (display block-info)))

(define (print-blockchain b)
  (displayln "+--------------------------------------------------------------------------------")
  (for ([block (blockchain-blocks b)])
    (print-block block)
    (display "+--------------------------------------------------------------------------------")
    (newline)))

(define (print-wallets b wallet-a wallet-b)
  (printf "\n=> Wallet A balance: ~a\n=> Wallet B balance: ~a\n\n"
          (blockchain/balance-wallet b wallet-a)
          (blockchain/balance-wallet b wallet-b)))

(provide (all-from-out "./src/blockchain.rkt")
         (all-from-out "./src/utils.rkt")
         (all-from-out "./src/peer-to-peer.rkt")
         format-transaction print-block print-blockchain print-wallets)

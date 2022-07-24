;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main.rkt                                                                                    ;;
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

(require "main-helper.rkt")

(when (file-exists? "blockchain.data")
  (begin
    (printf "Found 'blockchain.data', reading...\n")
    (print-blockchain (utils/file->struct "blockchain.data"))
    (exit)))

; Initialize wallets
(printf "> Initialize Wallets...\n")
(define coin-base (wallet/make))
(define wallet-a (wallet/make))
(define wallet-b (wallet/make))

; Transactions
(printf "> Initialize Blockchain...\n")
(define genesis-t (transaction/make coin-base wallet-a 100 '()))

; Unspent transactions (store our genesis transaction)
(define utxo (list (transaction-io/make 100 wallet-a)))

; Blockchain initiation
(define blockchain (blockchain/init genesis-t "1337cafe" utxo))
(print-wallets blockchain wallet-a wallet-b)

; Make a second transaction
(printf "--> Mining first transaction...\n")
(set! blockchain (blockchain/send-money blockchain wallet-a wallet-b 2 (utils/file->contract "contract.script")))
(print-wallets blockchain wallet-a wallet-b)

; Make a third transaction
(printf "--> Mining second transaction...\n")
(set! blockchain (blockchain/send-money blockchain wallet-b wallet-a 1 (utils/file->contract "contract.script")))
(print-wallets blockchain wallet-a wallet-b)

; Attempt to make a fourth transaction
(printf "--> Attempting to mine fourth (not-valid) transaction...\n")
(set! blockchain (blockchain/send-money blockchain wallet-b wallet-a 3 (utils/file->contract "contract.script")))
(print-wallets blockchain wallet-a wallet-b)

(printf "--> Blockchain is valid: ~a\n\n" (blockchain/valid? blockchain))

(for ([block (blockchain-blocks blockchain)])
  (print-block block)
  (newline))

(utils/struct->file blockchain "blockchain.data")
(printf "--> Exported blockchain to 'blockchain.data'...\n")

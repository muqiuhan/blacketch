;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main-p2p.rkt :                                                                              ;;
;; This is where we will put all the components together and use them. We                      ;;
;; want this implementation to accept some input arguments, such as the                        ;;
;; blockchain database file and the IP and port addresses of each peer                         ;;
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
(require racket/string)
(require racket/list)
(require racket/set)

(define args (vector->list (current-command-line-arguments)))

(when (not (= 3 (length args)))
  (begin
    (printf "Usage: main-p2p.rkt db.data port ip1:port1,ip2:port2...")
    (newline)
    (exit)))

(define (string-to-peer-info s)
  (let ([s (string-split s ":")])
    (peer-info (car s) (string->number (cadr s)))))

; Get args data
(define db-filename (car args))
(define port (string->number (cadr args)))
(define valid-peers (map string-to-peer-info (string-split (caddr args) ",")))

; Try to read the blockchain from a file (DB), otherwise create a new one
(define db-blockchain
  (if (file-exists? db-filename)
      (utils/file->struct db-filename)
      (initialize-new-blockchain)))

; Create a new wallet for us to use
(define wallet-a (wallet/make))

; Creation of new blockchain
(define (initialize-new-blockchain)
  ; Initialize wallets
  (define coin-base (wallet/make))

  ; Transactions
  (printf "Making genesis transaction...\n")
  (define genesis-t (transaction/make coin-base wallet-a 100 '()))
  
  ; Unspent transactions (store our genesis transaction)
  (define utxo (list (transaction-io/make 100 wallet-a)))

  ; Blockchain initiation
  (printf "Mining genesis block...\n")
  (blockchain/init genesis-t "1337cafe" utxo))

(define peer-context (peer-context-data "Test peer" port (list->set valid-peers) '() db-blockchain))
(define (get-blockchain) (peer-context-data-blockchain peer-context))

(peer-to-peer/run peer-context)

; Keep exporting the database to have up-to-date info whenever a user quits the app.
(define (export-loop)
  (sleep 10)
  (utils/struct->file (get-blockchain) db-filename)
  (printf "Exported blockchain to '~a'...\n" db-filename)
  (export-loop))

(thread export-loop)

; Procedure to keep mining empty blocks, as the p2p runs in threaded mode.
(define (mine-loop)
  (let ([newer-blockchain (blockchain/send-money (get-blockchain) wallet-a wallet-a 1 (utils/file->contract "contract.script"))])
    (set-peer-context-data-blockchain! peer-context newer-blockchain)
    (printf "Mined a block!")
    (sleep 5)
    (mine-loop)))

(mine-loop)

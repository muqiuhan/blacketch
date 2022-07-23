;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; block.rkt : Defines how a wallet works                                         ;;
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

(require crypto)
(require crypto/all)

;; A wallet is structure that contains a public and a private key.
(struct wallet
  (private-key public-key)
  #:prefab)

;; Create a wallet by generating random public and private keys, and it will rely on the RSA algorithm.
(define (wallet/create)
  (letrec ([rsa-impl (get-pk 'rsa libcrypto-factory)]
           [prikey (generate-private-key rsa-impl '((nbits 512)))]
           [pubkey (pk-key->public-only-key prikey)])
    (wallet
     (bytes->hex-string (pk-key->datum prikey 'PrivateKeyInfo))
     (bytes->hex-string (pk-key->datum pubkey 'SubjectPublicKeyInfo)))))

(provide (struct-out wallet)
         wallet/create)

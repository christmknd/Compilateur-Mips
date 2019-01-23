#lang racket/base

(require "parser.rkt"
         "semantics.rkt"
         "stdlib.rkt"
         "ast.rkt"
         "minicomp.rkt"
         "instr.rkt")

(define argv (current-command-line-arguments))
(cond
  ((>= (vector-length argv) 1)
   (define in (open-input-file (vector-ref argv 0)))
   (port-count-lines! in)
   (define parsed (liec-parser in))
   (close-input-port in)
   ;(printf "Parsing ok.\n")
   ;(displayln parsed)

   (define prog (check-exprs parsed *stdlib-types* Any))
   ;(printf "Typing ok.\n")
   ;(displayln (car prog))

  (for-each mips-emit
          (append
           ;; On initialise notre environnement local :
           (list (Move 'fp 'sp))
           ;; On compile une expression :
           (comp (car (car prog))
                 ;; avec un environnement vide :
                 (make-immutable-hash)
                 ;; et fp-sp = 0 (vu que fp = sp à ce moment là) :
                 0)
           ;; On affiche le résultat, qui est dans v0
           (list (Move 'a0 'v0)
               
                 (Li 'v0 1) ;; 4 pour print_string qui est le type du résultat
                 ;; affichage retour à la ligne :
                 (Syscall)
                 ;; main return 0
                 (Li 'v0 0)
                 (Jr 'ra)))))


  (else
   (eprintf "Usage: racket liec.rkt <source.liec>\n")
   (exit 1)))

#lang racket/base

(require racket/match
         "ast.rkt"
         "instr.rkt")

(provide mips-emit)
(provide comp)

;;;;; minicomp
;; la convention utilisée dans ce compilateur est
;; de toujours mettre la valeur calculée dans $v0

(define (comp ast env fp-sp) ;; le décalage entre sp et fp est fp - sp
  (match ast


    ((Const n)
     ;; Constante entière mise dans v0
     (list (Li 'v0 n)))




    ((Let n v )
     ;; Variable locale : let n = v in e
     (append
      ;; on compile v pour avoir sa valeur dans v0 :
      (comp v env fp-sp)
      ;; on empile la variable locale :
      (list (Addi 'sp 'sp -4)
            (Sw 'v0 (Mem 0 'sp)))
      ;; à partir de là fp - sp a grandi de 4 :
        ;; on compile e pour le mettre dans v0 :
      ))
    ((Var n)
     ;; Référence à une variable
     ;; on met la valeur de la variable dans v0 :
     (list (Lw 'v0 (hash-ref env n))))))



(define (mips-loc loc)
  (match loc
    ((Lbl l)   (format "~a" l))
    ((Mem b r) (format "~a($~a)" b r))))

(define (mips-emit instr)
  (match instr
    ((Move rd rs)   (printf "move $~a, $~a\n" rd rs))
    ((Li r i)       (printf "li $~a, ~a\n" r i))
    ((La r a)       (printf "la $~a, ~a\n" r (mips-loc a)))
    ((Addi rd rs i) (printf "addi $~a, $~a, ~a\n" rd rs i))
    ((Sw r loc)     (printf "sw $~a, ~a\n" r (mips-loc loc)))
    ((Lw r loc)     (printf "lw $~a, ~a\n" r (mips-loc loc)))
    ((Syscall)      (printf "syscall\n"))
    ((Jr r)         (printf "jr $~a\n" r))
    ((Label l)      (printf "\t~a:\n" l))))

(define (mips-data data)
  (printf ".data\n")
  (hash-for-each data
                 (lambda (k v)
                   (printf "~a: .asciiz ~s\n" k v)))
  (printf "\n.text\n.globl main\nmain:\n"))


(mips-data (make-hash '((str_123 . "coucou") (nl . "\n"))))

#lang racket
(require 2htdp/batch-io)
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

;: Pablo Cesar Jimenez Villeda - A0170357
;: Ariann Fernando Arriaga Alcantara - A01703556

;: ACTIVIDAD INTEGRADORA 3.4 RESALTADOR DE SINTAXIS (EVIDENCIA DE COMPETENCIA)

(define input-file "input.txt")
(define output-file "output.html")
(define head-html "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"UTF-8\"><meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\"><link rel=\"stylesheet\" href=\"./styles.css\"><title>Actividad integral</title></head><body><div class=\"tokens\"><h3 class=\"variable\">Variables</h3><h3 class=\"palabraReservada\">Palabra Reservada</h3><h3 class=\"operand\">Operadores</h3><h3 class=\"integer\">Enteros</h3><h3 class=\"real\">Reales</h3><h3 class=\"string\">Cadena</h3><h3 class=\"comment\">Comentarios</h3></div><br>")

(define rgx_matcher #rx"\\/\\/.*|\".*\"|\\#include|\\scanf|\\printf|\\auto|\\else|\\long|\\switch|\\break|\\enum|\\register|\\typedef|\\case|\\extern|\\return|\\union|\\char|\\float|\\short|\\unsigned|\\const|\\for|\\signed|\\void|\\continue|\\goto|\\sizeof|\\volatile|\\default|\\if|\\stati|\\while|\\do|\\int|\\struct_Packed|\\double|\\-*[0-9]+\\.[0-9]+((E|e)-*[0-9]*)*|[a-zA-Z][a-zA-Z0-9_]*|\\-*[0-9]+|\\^|\\/|\\&|\\||\\+|\\-|\\*|\\=|\\(|\\)|.")

;; detect-token: string -> string
(define (detect-token l)
    (cond
      [(regexp-match? #rx"\\/\\/.*" l) (string-append "<span class=\"comment\">" l "</span>")]
      [(regexp-match? #rx"\".*\"" l) (string-append "<span class=\"string\">" l "</span>")]
      [(regexp-match? #rx"\\#include|\\scanf|\\printf|\\auto|\\else|\\long|\\switch|\\break|\\enum|\\register|\\typedef|\\case|\\extern|\\return|\\union|\\char|\\float|\\short|\\unsigned|\\const|\\for|\\signed|\\void|\\continue|\\goto|\\sizeof|\\volatile|\\default|\\if|\\stati|\\while|\\do|\\int|\\struct_Packed|\\double" l) (string-append "<span class=\"palabraReservada\">" l "</span>")]
      [(regexp-match? #rx"\\-*[0-9]+\\.[0-9]+((E|e)-*[0-9]*)*" l) (string-append "<span class=\"real\">" l "</span>")]
      [(regexp-match? #rx"[a-zA-Z][a-zA-Z0-9_]*" l) (string-append "<span class=\"variable\">" l "</span>")]
      [(regexp-match? #rx"\\-*[0-9]+" l) (string-append "<span class=\"integer\">" l "</span>")]
      [(regexp-match? #rx"\\^|\\/|\\&|\\|\\+|\\-|\\*|\\=|\\(|\\)" l) (string-append "<span class=\"operand\">" l "</span>")]
      [else (string-append "<span>" l "</span>")]
      ))

;; lexer-line: string -> string
(define (lexer-line lst)
  (cond
    [(null? lst) "</br>"]
    [else (string-append (detect-token (car lst)) (lexer-line (cdr lst)))]))

;; lexer-lines: list -> string
(define (lexer-lines lst)
  (cond
    [(null? lst) ""]
    [else (string-append (lexer-line (regexp-match* rgx_matcher (car lst))) (lexer-lines (cdr lst)))]))

;; lexer: string string -> string
(define (lexer if of)
  (write-file of (string-append head-html (lexer-lines (read-lines if)) "</body></html>")))
  
;; TEST
 (lexer input-file output-file) 
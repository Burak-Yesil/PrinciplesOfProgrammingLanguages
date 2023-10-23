
(* The type of tokens. *)

type token = 
  | UNTUPLE
  | TL
  | TIMES
  | THEN
  | SEMICOLON
  | RPAREN
  | RBRACE
  | RANGLE
  | PROC
  | PLUS
  | OF
  | NODE
  | MINUS
  | LPAREN
  | LET
  | LBRACE
  | LANGLE
  | ISZERO
  | INT of (int)
  | IN
  | IF
  | ID of (string)
  | HD
  | EQUALS
  | EOF
  | EMPTYTREE
  | EMPTYLIST
  | EMPTY
  | ELSE
  | DOT
  | DIVIDED
  | CONS
  | COMMA
  | CASET
  | ARROW
  | ABS

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)

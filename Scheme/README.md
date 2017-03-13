# Programming-Languages
SCHEME functions LOG:

$ ./ugly-regexp/parser.rkt 
(chars(a) . chars(b)) + *chars(c) 
((([a][b]))|[c]*) 

(chars(a) . chars(b)) + chars(c)* 
<stdin>:2:32: syntax error at ’*’, expecting ’<NL>’ 

chars(a) . chars(b) + *chars(c) 
([a]([b]|[c]*)) 

$ racket Welcome to Racket v6.1. > (load "fns.rkt") 

>(quadratic-roots 3 -6 3) 
’(1 1) 
> (quadratic-roots 1 -10 34) 
’(5+3i 5-3i) 
> (mul-list ’() 22) 
’() 
> (mul-list ’(1 2 5) 22) 
’(22 44 110) 
> (sum-lengths 
’((1 2 (3 4)) ((5 6)) ())) 4 
> (sum-lengths ’((1) (2 (3 . 4)))) 
3 
> (poly-eval ’(1 2 3 4) 3) 
58 
> (poly-eval-horner ’(2 3 4) 4) 
48 
> (count-occurrences ’((a b c) a (c (d a) b)) ’a) 
3 
> (count-occurrences ’((a b c) a (c (d a) b)) ’(d a)) 
1 
> (count-occurrences ’() ’a) 
0 
> (eval-arith ’(add (mul 8 5) 2)) 
42 
> (eval-arith ’(sub (mul 11 5) 13)) 
42 
> (sum-lengths-tr ’(((b c) a) () (c d))) 
4 
> (poly-eval-tr ’() 5) 
0 
> (poly-eval-tr ’(4 3 2 1) 5) 
586 
> (poly-eval-tr ’(1 1 2 0) 3) 
42 
> (mul-list-2 ’() 5) 
’() 
> (mul-list-2 ’(9 3 1) 5) 
’(45 15 5) 
> (sum-lengths-2 ’(((((1)))) (2)))
2

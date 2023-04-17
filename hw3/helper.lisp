; ***********************************************************
; *  341 Programming Languages                              *
; *  Fall 2022                                              *
; *  Author: Omer Faruk Akduman                             *
; *  File  : gpp_lexer.lisp 				                *
; * 										                *
; *  Lexical Syntax Analyser for G++ programming language.	* 
; ***********************************************************



(defparameter current-line "")
(defparameter current-line-list '())
(defparameter exxit nil);to exit loop
(defparameter comment-line-found nil);if comment line found exit loop
(defparameter string-symbol-founded nil);if comment line found exit loop
(defparameter first-comment-symbol-nth 0)
(defparameter second-comment-symbol-nth 0)
(defparameter lexical-error-list '())
(defparameter is-there-lexical-error nil)
(defparameter is-there-parantheses-error nil)


(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line
          )))

(defun list-to-string2 ( lst  )
    "A method that to translate list to string"
    (if (cdr lst)
        (concatenate 'string (car lst) " " (list-to-string2 (cdr lst) ))
        (car lst)
    )
)

(defparameter key-hash (make-hash-table :test 'equal));test 'equal to string -> string
(setf token-string "+ - # * * , ` ! $ ; : ?")
(defparameter return-list '())

;;some of tokens placed in the hash map
;Hash-map used for algorithm efficiency 
;acces an any token is O(1)
(setf (gethash "and" key-hash) "KW_AND")
(setf (gethash "or" key-hash) "KW_OR")
(setf (gethash "not" key-hash) "KW_NOT")
(setf (gethash "eq" key-hash) "KW_EQUAL")
(setf (gethash "equal" key-hash) "KW_EQUAL")
(setf (gethash "gt" key-hash) "KW_GT")
(setf (gethash "less" key-hash) "KW_LESS")
(setf (gethash "nil" key-hash) "KW_NIL")
(setf (gethash "list" key-hash) "KW_LIST")
(setf (gethash "append" key-hash) "KW_APPEND")
(setf (gethash "concat" key-hash) "KW_CONCAT")
(setf (gethash "set" key-hash) "KW_SET")
(setf (gethash "deffun" key-hash) "KW_DEFFUN")
(setf (gethash "for" key-hash) "KW_FOR")
(setf (gethash "if" key-hash) "KW_IF")
(setf (gethash "while" key-hash) "KW_WHILE")
(setf (gethash "exit" key-hash) "KW_EXIT")
(setf (gethash "load" key-hash) "KW_LOAD")
(setf (gethash "disp" key-hash) "KW_DISP")
(setf (gethash "true" key-hash) "KW_TRUE")
(setf (gethash "True" key-hash) "KW_TRUE")
(setf (gethash "False" key-hash) "KW_FALSE")
(setf (gethash "+" key-hash) "OP_PLUS")
(setf (gethash "-" key-hash) "OP_MINUS")
(setf (gethash "/" key-hash) "OP_DIV")
(setf (gethash "*" key-hash) "OP_MULT")
(setf (gethash "(" key-hash) "OP_OP")
(setf (gethash ")" key-hash) "OP_CP")
(setf (gethash "**" key-hash) "OP_DBLMULT")
(setf (gethash "," key-hash) "OP_COMMA")


(defun txt-process ()
    "a function to process with txt file"
    (setf txt-list (get-file (car *args*)))
    (setf current-line (list-to-string2 txt-list ))
    (setf current-line (paranthese-arrange current-line))
    (setf current-line-list (my-split current-line))

    (if (equal (car current-line-list) ":exit")
        (setf exxit t)
        (iter-recursive current-line-list 0)
    )
    ; (print-return-list)
)  

(defun give-syntax-error (word)
"A method that gives syntax error" 
(add-element-return-list word "LEXICAL ERROR:")
)



(defun add-element-return-list (word word-type)
    (if (eq return-list nil)
        (push word return-list)
        (push  word (cdr (last return-list))))
    
    (push word-type (cdr (last return-list)))
)

(defun print-return-list()
(print return-list)
(setf return-list '()))


;;to split given string
(defun my-split (string &key (delimiterp #'delimiterp))
  "A method that split the string"
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))


(defun delimiterp (c) (char= c #\Space))

;;to split given string
(defun my-split-float (string &key (delimiterp-float #'delimiterp-float))
  "A method that split the valuef to float number"
  (loop :for beg = (position-if-not delimiterp-float string)
    :then (position-if-not delimiterp-float string :start (1+ end))
    :for end = (and beg (position-if delimiterp-float string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))


(defun delimiterp-float (c) (char= c #\f))




(defun get-line ()
    (setf comment-line-found nil)
    (terpri)
    (terpri)
    (princ ">>>  ")
  (setq current-line (read-line))
  (setf current-line (paranthese-arrange current-line))
   current-line)

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun paranthese-arrange (word) 
    "A method that arrange parantheses"
    (replace-all (replace-all word ")" " ) ") "(" " ( ")
    )

(defun iter-recursive(list n) 
    "iterate all element and check its tokens"
    
    (if (or (null list) comment-line-found  string-symbol-founded)
        (if (equal string-symbol-founded t)
            (progn (setf string-symbol-founded nil)
                    (setf current-line (subseq current-line (+ second-comment-symbol-nth 1)))
                    ; (setf line-taken T)
                    (process-a-line t)
            )
            t)
        (progn 
            (if (eq (gethash (car list) key-hash) nil) 
                    (general-error (car list) n) ;if there is no token such that goes identifier type
                    (add-element-return-list (car list) (gethash (car list) key-hash)) )
            (iter-recursive (cdr list) (+ n 1))
        )
    )
)


(defun string-include (string1 string2)
    "A method that checks whether string is include or not"
  (let* ((string1 (string string1)) (length1 (length string1)))
    (if (zerop length1)
        nil 
        (labels ((sub (s)
                   (cond
                    ((> length1 (length s)) nil)
                    ((string= string1 s :end2 (length string1)) string1)
                    (t (sub (subseq s 1))))))
          (sub (string string2))))))

(defun found-comment ()
    (add-element-return-list (subseq current-line (search ";;" current-line)) "COMMENT")
    (setf comment-line-found T)
    (setf current-line-list '(nil))
    )

(defun is-contain-tokens-helper (word n) 
    (if (equal (not (string-include (char word n) token-string)) nil)
        (if (and (< 1 (length word)) (and (equal (char word 0) #\;) (equal (char word 1) #\;)))
            (found-comment)
            (give-syntax-error word)) 
        (if (> (- (length word) 1) n)
            (is-contain-tokens-helper word (+ n 1))
            nil));if there is no token 
)



(defun is-integer-or-float (word)
    (cond   ((equal (char word 0) #\0) (give-syntax-error word) )
            ((every #'digit-char-p word) (add-element-return-list word "VALUEI"))
            ((and (search "f" word) (not (or (eq (char word 0) #\f) (eq (char word (- (length word) 1)) #\f))) (every #'digit-char-p (substitute #\0 #\f word))) 
                (add-element-return-list word "VALUEF")))
    )

(defun is-contain-tokens (word) (is-contain-tokens-helper word 0) )

(defun string-symbol-found (n)
    (setf first-comment-symbol-nth (search (nth n current-line-list) current-line))
    (setf second-comment-symbol-nth (search "\""  current-line :start2 (+ first-comment-symbol-nth 1)))
    (if (equal second-comment-symbol-nth nil)
        (give-syntax-error (subseq current-line (+ first-comment-symbol-nth 1) ))
        (progn  
            (add-element-return-list (subseq current-line (+ first-comment-symbol-nth 1) second-comment-symbol-nth) "STRING")
            (setf string-symbol-founded T)
        )
    )
)
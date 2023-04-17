(load "helper.lisp")

(defparameter id-hash (make-hash-table :test 'equal));test 'equal to string -> string
(defparameter func-hash (make-hash-table :test 'equal));test 'equal to string -> string


;#run inter preter
; (defun run-interpreter ()
;     (run-interpreter-start)
;     (print return-list)
;     (run-interpreter)
;     )
;;
; (run-interpreter)

;;paranthese check

(defun lexical-error-found (lexical-error-word)
    "a method check is there any syntax error"
    (print lexical-error-word)
    (setq lexical-error-list (append lexical-error-list (list lexical-error-word)))
    (setf is-there-lexical-error T)
)



(defun lexical-error-check(listt)
    (if (null listt)
        nil
        (if (equal (car listt) "LEXICAL ERROR:")
            T
            (lexical-error-check(cdr listt)))
    )
)

(defun lexical-error-check(listt n)
    "after tokenize check is there any lexical error or not"
    (if (equal (length listt) (- n 1))
        nil
        (if (equal (nth n listt) "LEXICAL ERROR:")
            (if (< n 0)
                t
                (lexical-error-found (nth (- n 1) listt))
            )
            (lexical-error-check listt (+ 1 n))
        )
    )
)


(defun check-paranthese (listt paranthese-number)
    "a method that checks if the parantheses' equal or not
    if paranthese-number positive then ( 'open paranthes' is more than  ) 'close paranthes' )
    else if paranthese-number is negative than ( 'open paranthes' is less than  ) 'close paranthes' )
    else parantheses are equal then return 0"
    (cond ((null listt) paranthese-number)
          ((equal (car listt) "OP_OP") (check-paranthese (cdr listt) (+ paranthese-number 1)))
          ((equal (car listt) "OP_CP") (check-paranthese (cdr listt) (- paranthese-number 1)))
          (t (check-paranthese (cdr listt) paranthese-number))
    )
)

(defun is-there-parantheses-error-f (listt)
    "Wrapper function to check-paranthese"
    (if (equal (check-paranthese listt 0) 0)
        nil
        (setf is-there-parantheses-error T))
)

(lexical-error-check return-list 0)
; (print-skr return-list)
; (print is-there-lexical-error)

; (print lexical-error-list)


; (print-skr return-list)
; (is-there-parantheses-error-f return-list)
; (print is-there-parantheses-error)


;  Input  ->  

(defun general-concrete-syntax (input)
    (cond 
          ((is-fdef input) (fdef-process input))            ;A) if input is function definition
          ((rule1 input) )                                  ;B) if input is rule 1
          ((is-explist input) (explist-process input))
          ((is-expb input) (expb-process input))            ;C) if input binary expression
          (t (print "error"))
          )
)

(defun explist-process-return-exps (input)
"A method that finds and returns expressions as a list"
    (let ((explist '())(continue t)(start 2))
        (loop 
            (if (equal (get-expression input start) nil)
                (setf continue nil)
                (progn 
                    (setf explist (append explist  (list (get-expression input start))))
                    (setf start (get-next-start-position input start))
                )
                )
        (when (equal continue nil) (return explist))
        )
    )
)

(defun explist-process-process-explist (exp-list)
"this function process explist one by one and return last evaluated one"
    (let ((returnx nil))
        (loop for x in exp-list
        do (progn(setf returnx (rule1 x))))
    
    returnx
    )
)
    

(defun explist-process (input)
"a method that make exp-list process"
;burda kaldik burda da explistleri listlere bolup gonderecez
    (let ((exp-list (explist-process-return-exps input)))
        
        (explist-process-process-explist exp-list)          
        )
)

(defun is-explist(input) 
    "A method that based Rule 1 B section"
    (cond   ((and (< 7 (length input)) (and (equal (nth 1 input) "OP_OP") (equal (nth 3 input) "OP_OP") )) t)
            (t nil);hicbirseye girmiyor aslinda hata var
            )
)


(defun is-expb(input) 
    "A method that based Rule 1 B section"
    (cond   ((and (< 2 (length input)) (or (equal (nth 3 input) "KW_EQUAL") (equal (nth 3 input) "KW_GT") (equal (nth 3 input) "KW_AND") (equal (nth 3 input) "KW_OR"))) t)
            ((and (< 2 (length input)) (equal (nth 3 input) "KW_NOT")) t)
            ((equal (nth 1 input) "KW_TRUE") t)
            ((equal (nth 1 input) "KW_FALSE") t)
            (t "nil");hicbirseye girmiyor aslinda hata var
            )
)

(defun expb-binary (input)

    (cond ((equal (nth 3 input) "KW_EQUAL") (print "KW_EQUAL"))
          ((equal (nth 3 input) "KW_GT") (print "KW_GT"))
          ((equal (nth 3 input) "KW_AND") (print "KW_EQUAL"))
          ((equal (nth 3 input) "KW_OR") (print "KW_AND"))
          ((equal (nth 3 input) "KW_NOT") (print "KW_OR"))

          (test-form-2 form2))
)


(defun expb-process-equal (input)
    (let ((lvalue nil )(rvalue nil) (lstart 4) (rstart -1))
        (setf lvalue (rule1 (get-expression input lstart) ))
        (setf rvalue (rule1 (get-expression input (get-next-start-position input lstart)) ))

        (equal lvalue rvalue)
    )        
)

(defun expb-process-gt (input)
    (let ((lvalue nil )(rvalue nil))
        (setf lvalue (rule1 (get-expression input 4) ))
        (setf rvalue (rule1 (get-expression input (get-next-start-position input 4)) ))

        (> lvalue rvalue)
    )        
)

(defun expb-process-and (input)
    (let ((lvalue nil )(rvalue nil) )
        (setf lvalue (expb-process (get-expression input 4) ))
        (setf rvalue (expb-process (get-expression input (get-next-start-position input 4)) ))

        (and lvalue rvalue)
    )        
)

(defun expb-process-or (input)
    (let ((lvalue nil )(rvalue nil) )
        (setf lvalue (expb-process (get-expression input 4) ))
        (setf rvalue (expb-process (get-expression input (get-next-start-position input 4)) ))

        (or lvalue rvalue)
    )        
)

(defun expb-process-not (input)
    (let ((value nil ))
        (setf value (expb-process (get-expression input 4) ))

        (not value)
    )        
)



(defun expb-process(input) 
    "A method that based Rule 1 B section"
    (cond   
            ((equal (nth 1 input) "KW_TRUE")    t)
            ((equal (nth 1 input) "KW_FALSE")   nil)
            ((equal (nth 3 input) "KW_EQUAL")   (expb-process-equal input))
            ((equal (nth 3 input) "KW_GT")      (expb-process-gt input))
            ((equal (nth 3 input) "KW_AND")     (expb-process-and input))
            ((equal (nth 3 input) "KW_OR")      (expb-process-or input))
            ((equal (nth 3 input) "KW_NOT")     (expb-process-not input))

            (t nil)
            )
)

;Rule 1 Exp -> A a1 (+ Exp Exp) |
;                a2 (- Exp Exp) |
;                a3 (* Exp Exp) |
;                a4 (/ Exp Exp) |
;              B b1 VALUEI      |
;                b2 VALUEF      |
;              C    FCALL       |
;              D    ASG
(defun rule1(EXP)
    (cond   ((is-value EXP) (is-value EXP));rule 1 part b
            ((is-operation EXP ) (operation-process EXP)) ;rule 1 part A
            ((is-assign EXP ) (assign-process EXP)) ;rule 1 part D
            ((is-id EXP ) (id-process EXP)) ;rule 1 part D
            ((is-fcall EXP) (fcall-process EXP))  
            ((is-if EXP) (if-process EXP))
            ((is-while EXP) (while-process EXP))
        (t nil)
    )
)

(defun is-while (EXP)
    (if (and (equal (nth 1 EXP) "OP_OP") (equal (nth 3 EXP) "KW_WHILE") )
        t
        nil)
    )
(defun while-process (EXP)
    (let ((expb '()) (exp-list '()) (start 4)(returnx nil))

        (setf expb (get-expression EXP start))
        (setf start (get-next-start-position EXP start))
        (setf exp-list (get-expression EXP start))

        (if (expb-process expb)
            ( loop
                (setf returnx (explist-process exp-list) )
                (when (equal (expb-process expb) nil) (return returnx))
            )
            nil
        )
    )
) 


(defun is-if (EXP)
    (if (and (equal (nth 1 EXP) "OP_OP") (equal (nth 3 EXP) "KW_IF") )
        t
        nil)
    )

(defun if-process (EXP)
    ; (print "this is if" )
    (let ((expb '()) (expf '()) (exps '()) (start 4))
        
        (setf expb (get-expression EXP start))
        (setf start (get-next-start-position EXP start))
        (setf expf (get-expression EXP start))
        (setf start (get-next-start-position EXP start))
        (setf exps (get-expression EXP start))
                
        (if (expb-process expb)
            (explist-process expf)
            (explist-process exps)
            )
        )
    )


(defun parse-float (VALUEF)
"A method that to parse float"
    (let ((valuef-list (my-split-float VALUEF)))
        (/ (parse-integer (nth 0 valuef-list)) (parse-integer (nth 1 valuef-list)))))



(defun is-value(EXP) 
    "A method that based Rule 1 B section"
    (if (< 1 (length EXP))
        (cond   ((equal (nth 1 EXP) "VALUEI" ) (parse-integer (car EXP)))
                ((equal (nth 1 EXP) "VALUEF" ) (parse-float (car EXP)))    
                (t nil)
            )
        nil
    )
)

(defun is-assign(EXP) 
    "A method that based Rule 1 B section"
    (if (and (< 5 (length EXP)) (equal (nth 1 EXP) "OP_OP") (equal (nth 3 EXP) "KW_SET") (equal (nth 5 EXP) "IDENTIFIER"))
        t
        nil
    )
)
(defun is-fdef(EXP) 
    "A method that based Rule 1 B section"
    (if (and (< 14 (length EXP)) (equal (nth 1 EXP) "OP_OP") (equal (nth 3 EXP) "KW_DEFFUN") (equal (nth 5 EXP) "IDENTIFIER") (equal (nth 7 EXP) "OP_OP"))
        t
        nil
    )
)
(defun fdef-process (EXP)
    (let ((func_name (nth 4 EXP)) (content EXP))
        (setf (gethash func_name func-hash) content)
        t
    )

)


(defun is-fcall(EXP) 
    "A method that based Rule 1 B section"
    (if (and (< 7 (length EXP)) (equal (nth 1 EXP) "OP_OP") (equal (nth 3 EXP) "IDENTIFIER") (equal (nth 5 EXP) "OP_OP") )
        (if (gethash (nth 2 EXP) func-hash)
            t
            nil)
        nil
    )
)

(defun fcall-get-parameters (f-call-exp variable-count) ;from function call
        "a method that gets function from function call"
    (let ((exp '())(variables '()) (continue t)(op-op nil)(iter 6))
        ( cond  ((equal variable-count 0) (variables))
                ((equal variable-count 1) (progn (setf variables (append variables (rule1 (get-expression f-call-exp iter)))) variables))
                ((equal variable-count 2) (progn 
                                            (setf variables (append variables (list (rule1 (get-expression f-call-exp iter)))))
                                            (setf iter (get-next-start-position f-call-exp iter))
                                            (setf variables (append variables (list (rule1 (get-expression f-call-exp iter)))))
                                            variables              
                                            )
                                        )
                ((equal variable-count 3) (progn 
                                            (setf variables (append variables (list (rule1 (get-expression f-call-exp iter)))))
                                            (setf iter (get-next-start-position f-call-exp iter))
                                            (setf variables (append variables (list (rule1 (get-expression f-call-exp iter)))))
                                            (setf iter (get-next-start-position f-call-exp iter))
                                            (setf variables (append variables (list (rule1 (get-expression f-call-exp iter)))))
                                            variables              
                                            )                 
                                        )   
                (t variables)

        )
    )
)

(defun get-parameter-number (f-definition)
    "A method that gets argument counts of function definition"
    (let ((count-parameter 0) (iter 9) (continue t))
        (loop
            (if (equal (nth iter f-definition) "IDENTIFIER")
                (setf count-parameter (+ count-parameter 1))
                t
                )

            (if (equal (nth iter f-definition) "OP_CP")
                    (setf continue nil)
                    t)

            (setf iter (+ iter 2))
            (when (equal continue nil) (return count-parameter))
            )
    )
)

(defun fcall-process-return-ids (f-definition )
    "A method that gets argument counts of function definition"
    (let ((count-parameter 0) (iter 9) (continue t) (variables '()))
        (loop
            (if (equal (nth iter f-definition) "IDENTIFIER")
                (setf variables (append variables (list (nth (- iter 1) f-definition))))
                t
                )

            (if (equal (nth iter f-definition) "OP_CP")
                    (setf continue nil)
                    t)

            (setf iter (+ iter 2))
            (when (equal continue nil) (return variables))
            )
    )


)

(defun f-call-process-binding (ids variables variable-count)
    "a function that control binding process"
    (if (eq (length variables) (length ids))
        (cond ((eq (length ids) 1) (setf (gethash (car ids) id-hash) (car variables)))

              ((eq (length ids) 2) 
              
              (progn (setf (gethash (car ids) id-hash) (car variables))
                     (setf (gethash (nth 1 ids) id-hash) (nth 1 variables))       
              ))

              ((eq (length ids) 3) 
              
              (progn (setf (gethash (car ids) id-hash) (car variables))
                     (setf (gethash (nth 1 ids) id-hash) (nth 1 variables))
                     (setf (gethash (nth 2 ids) id-hash) (nth 2 variables))
              ))
              )

        nil)
)
(defun fdef-get-exp-list (fdef start)
    (let ((explist '())(continue t))
        (loop 
            (if (< (length fdef ) start)
                (setf continue nil)
                (progn 
                    (setf explist (append explist   (get-expression fdef start)))
                    (setf start (+ 1 start))
                )
                )
        (when (equal continue nil) (return explist))
        )
    )
    )
(defun fcall-process (f-call-exp)
    (let ((func_name (nth 2 f-call-exp)) (ids '()) (variables '()) (exp-list '())(content '()) (variable-count 0))
        (setf content (gethash func_name func-hash))
        (setf variable-count (get-parameter-number content) )

        
        (setf ids (fcall-process-return-ids content ))

        (if (equal variable-count 0)
            nil
            (setf variables (fcall-get-parameters f-call-exp variable-count))
        )
        ; (print "baner2")

        (if (not(listp variables))
            (setf variables (list variables))
            nil)
        
        ; (print variables)
        
        ;;;;;;;;;;;;;variableslar flatten liste seklinde olmali
        ;;
        ;;
        ;;Bak yukarida parametreleri aldik f-definition dan
        ;;degerleri aldik f-call dan
        ;;asagida da global hasp mape atiyacagiz bu recursionlarda
        ;;ve scoping de hata verir
        ;;
        ;;
        (f-call-process-binding ids variables variable-count) ;binding variables
        ; (print "baslar haci")
        (setf exp-list (fdef-get-exp-list content (+ 10 (* variable-count 2))))
        ; (print exp-list)

        (explist-process exp-list)
        )
)






(defun assign-process (EXP)
    "A method that process to assign"
    (let ((lvalue (rule1 (get-expression EXP 6)))(local-identifier (nth 4 EXP)))
        (setf (gethash local-identifier id-hash) lvalue)
        )

)

(defun general-error (word n)
    "a function works as dfa to check general-errors"
    (if (eq (is-contain-tokens word) nil)
        (if (eq (is-integer-or-float word) nil)
            (if (not (equal (char word 0) #\"))
                (progn 
                (add-element-return-list word "IDENTIFIER"))
                (string-symbol-found n))
            nil
            ) )    ;if there is no error then its identifier
        nil)

(defun process-a-line (&optional (line-taken nil)) 
    (if (eq line-taken t)
        (setf line-taken nil)
        (get-line))
    
    (setf current-line-list (my-split current-line))
    (if (equal (car current-line-list) ":exit")
        (setf exxit t)
        (iter-recursive current-line-list 0)
    )
)

(defun run-interpreter-start ()
    "A method that get input and check lexical analysis"
    (setf return-list '())
    (if *args*
        (progn (txt-process) (setf exxit T) )
        (progn   (if (eq return-list nil)
                t
                (print-return-list))
        
        (if (eq exxit nil)  ;if print :exit program is terminate
        (process-a-line)
        t
    ))
    )
)

(princ "Welcome to interpreter and lexical analyzer of the g++ that created by Omer F. Akduman")
(terpri)
(princ "to quit input :exit")
; (run-interpreter)

; (print return-list)



(defun find-the-string-first-occurence-index(a-list wanted-string start-index)
    "A method that finds first index of given string in a list"
    (if (> start-index (- (length a-list) 1))
        nil
        (if (equal wanted-string (nth start-index a-list))
            start-index
            (find-the-string-first-occurence-index a-list wanted-string (+ 1 start-index)))
        )
)



(defun get-sublist (alist start stop)
    "a method that get sublist from main list"
    (let ((sub-list '()))
        (loop 
            (setq sub-list (append sub-list (list (nth start alist))))

            (setq start (+ start 1))
            (when (> start stop) (return sub-list))
            )
        )
)



(defun get-expression (alist start)
    "a method that get sublist from main list
     alist: return-list (main-list)
     start: starting index of ( character
     "
    
    (if (equal (nth start alist) ")")
        nil
        (let ((sub-list '()) (open-paranthese 0))
        (loop 
            (if (equal (nth start alist) "(")
                (setf open-paranthese (+ open-paranthese 1))
                (if (equal (nth start alist) ")")
                    (setf open-paranthese (- open-paranthese 1))
                    nil
                )
            )
            (setq sub-list (append sub-list (list (nth start alist))))
            (setq sub-list (append sub-list (list (nth (+ 1 start) alist))))

            (setq start (+ start 2))
            (when (equal open-paranthese 0) (return sub-list))
            )
        ))
)

(defun get-next-start-position (alist start)
    "a method that get sublist from main list
     alist: return-list (main-list)
     start: starting index of ( character
    "
    (if (< (length alist) start)
        nil
        (let ( (open-paranthese 0))
        (loop 
            (if (equal (nth start alist) "(")
                (setf open-paranthese (+ open-paranthese 1))
                (if (equal (nth start alist) ")")
                    (setf open-paranthese (- open-paranthese 1))
                    nil
                )
            )
            (setq start (+ start 2))
            (when (equal open-paranthese 0) (return start))
            )
        )    
    )
    
)


(defun is-operation (EXP)
    "A method that check expression start with ( +|-|*|/ "
    (if (and (equal (nth 1 EXP) "OP_OP") (or (equal(nth 3 EXP) "OP_PLUS") (equal(nth 3 EXP) "OP_MINUS") (equal(nth 3 EXP) "OP_DIV") (equal(nth 3 EXP) "OP_MULT")))
        t
        nil)
)

(defun operation-process (EXP)
    (let ((lvalue nil )(rvalue nil) (lstart 4) (rstart -1))
        (setf lvalue (rule1 (get-expression EXP lstart) ))
        (setf rvalue (rule1 (get-expression EXP (get-next-start-position EXP lstart)) ))

        (cond ((equal (nth 3 EXP) "OP_PLUS") (+ lvalue rvalue))
              ((equal (nth 3 EXP) "OP_MINUS") (- lvalue rvalue))
              ((equal (nth 3 EXP) "OP_DIV") (/ lvalue rvalue))
              ((equal (nth 3 EXP) "OP_MULT") (* lvalue rvalue))
              )
        )

    )
(defun is-id (EXP)
    "a method that check is expression id or not"
    (if (equal (nth 1 EXP) "IDENTIFIER")
        t
        nil)
)

(defun id-process (EXP)
    "a method that check is expression id or not"
    (gethash (car EXP) id-hash)
)

(defun count-of (listt key) 
    "a function that finds occurence in given list"
    (let ((count 0))
        (loop for x in listt
            do( if (equal x key)
                (setf count (+ count 1))
                nil))
            count
        )
)

;::::::::::::::::::::::::::SYNTAX CHECK PART::::::::::::::::::::::::

(defun check-parantheses (return-list)
    "a method that check paranthese equality"
    (if (and (eq 2 (length return-list)))
        t
        (if (equal (count-of return-list "OP_OP")  (count-of return-list "OP_CP"))
            t
            nil
        )
    )
)

(defun is-contain-lexical-error (return-list)
    (if (< 0 (count-of return-list "LEXICAL ERROR:"))
        t
        nil)
)

(defun is-contain-exit (return-list)
    (if (or (< 0 (count-of return-list ":exit")) (< 0 (count-of return-list "KW_EXIT")))
        t
        nil)
    )




(defun run-interpreter ()

    (run-interpreter-start)
    
    (cond ((not (check-parantheses return-list) ) (progn (setf exxit t) (print "paranthese error")))
          ((is-contain-exit return-list ) (progn (setf exxit t) (print "Exit normally")))
          ((is-contain-lexical-error return-list ) (progn (setf exxit t) (print "Lexical Error")))
          (t (print(general-concrete-syntax return-list)))
          )

    (if (not exxit)
        (run-interpreter)
        nil)
)

(run-interpreter)
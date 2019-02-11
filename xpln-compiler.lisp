;;;; this example compiles arithmetic expressions and their assignment to 
;;;; Three address code (TAC) then to register-only ops the  to MIPS target code
;;;; -cem bozsahin
;;;;
;;;; CENG 444 - Assignment 2
;;;; Ahmet Sadik Gulden
;;;;
;;;; 1902824
;;;;

;; I added a sequencing rule make assignments one after another.

;; Examples in tac and zac directories generate IC code with IC temporaries.

;; This version generates TAC with IC temporaries, then creates MIPS-style .data and .text segments
;; which refer to these temporaries, and implements the TAC as register operations.
;; This is because in MIPS all ops are done in registers; no op refers to memory locations.
;; For example (3ac mult a b c) meaning a := b * c in IC code is translated into
;;  l.s $f0,b
;;  l.s $f2,c
;;  mul.s $f0,$f0,$f2
;;  s.s $f0,a

;; NOTE:
;; We do not use conversion commands like cvt.s.w because we map all source integers to float 
;; at the IC level.
;; 
;; Because MIPS does not allow mixed arithmetic, we map integer constants to integer registers and make them float
;;
;; where l.s is load word from memory to register
;; and s.s means save the register's value in a word in memory.
;; $fi are temporary floating-point value registers of MIPS-like architectures. All we need is 2 for TAC codes.
;; We can think of this version as a register-based TAC, rather than true target code.

;; Here is the algorithm: 1) Generate TAC with relative addresses with IC temporaries. 2) Map that result to a 
;;  register-based TAC with data and code segments. 

;; A Symbol table keeps names (variables and constants) and type info. Its format is
;; hash key: (name blockno)  hash value: (type value)
;;   type is NUM or VAR.  Value is relevant for numbers to choose 'li' MIPS command later. Each type takes a fixed amount of 
;;   space (in MIPS generation i assume one word).

;; Forward declerations to silence warnings

(declaim (ftype (function (list) t) create-partial-code-segment))
(declaim (ftype (function (list) t) var-get-code))
(declaim (ftype (function (list) t) var-get-place))
(declaim (ftype (function (list) t) mk-code))
;;

(defun mk-symtab (size)
  (make-hash-table :test #'equal :size size :rehash-threshold 0.8))  ; we need equal function to match a LIST of values as key

(defparameter *symtab* (mk-symtab 20000)) ; this is a global variable, referred to in the grammar's semantic actions

(defparameter *funtab* (mk-symtab 2000))  ; holds function-blockno table.

(defparameter *maxblockno* 0)  ; increment this everytime a new code block (procedure etc.) is entered.

(defparameter *blockno* 0)  ; current index of block

(defparameter *labelno* 0)  ; increment this everytime a new generic label is created. 

(defparameter *localvariables* '()) ; updated each time function definition rule reduce.

(defparameter *outstream* nil) ; to control output to file or standard output

(defun target-code-mips (input &optional (outp nil))
  "if outp is t, will send it to std out"
  (clrhash *symtab*) ; we need to reset the symbol table for every code gen
  (clrhash *funtab*)
  (setf *maxblockno* 0)
  (setf *blockno* 0)
  (setf *labelno* 0)
  (if (or (listp input) (not outp))
    (setf *outstream* nil)
    (setf *outstream* (concatenate 'string "target_" input ".s")))
  (format t "~%Lexical analyzer feed to parser as seen by Lisp reader:~2%~A" 
      (with-open-file (s input :direction :input :if-does-not-exist :error)(read s)))
  (target-code input))

(defun mk-sym-entry-to-table (name blockno)
  "NB: Lisp hash is collision-free, duplicates just replace the older value."
  (cond ((floatp name) (setf (gethash (list name 0) *symtab*) (list 'float name)))
        ((integerp name) (setf (gethash (list name 0) *symtab*) (list 'int name)))
        ((symbolp name) (setf (gethash (list name blockno) *symtab*) (list 'var (format nil "~A_~d" name blockno))))
        (t (setf (gethash (list name blockno) *symtab*) (list 'unknown (format nil "~A_~d" name blockno))))))

(defun mk-sym-entry-to-next-block (name)
    (mk-sym-entry-to-table name (+ 1 *maxblockno*)))

(defun mk-sym-entry (name)
    (mk-sym-entry-to-table name *blockno*))

(defun mk-sym-entry-if-constant (value)
    (if (not (symbolp value)) (mk-sym-entry value)))

(defun sym-get-type (val)
  (first val))

(defun sym-get-value (val)
  (second val))

(defun get-sym-at-block (name blockno)
  (if (> blockno 0)
      (let ((sym (gethash (list name blockno) *symtab*)))
            (if sym (sym-get-value sym) (get-sym-at-block name 0)))
      (sym-get-value (gethash (list name 0) *symtab*))))

(defun get-sym (name)
    (get-sym-at-block name *blockno*))

;; SDD section
;;
;; advice: never use a constant on the RHS of rules, put them in the lexicon and 
;;         symbolize them in lexforms

;;; TAC Templates:
;;  (3ac op p1 p2 p3)
;;  (2ac op p1 p2)
;;  (2copy p1 p1)
;;  (bool op p1 p1)

;; All of below instructions are floating point instructions(.s means float)
;; intstruction set correlation.
;; MIPS is case-sensitive, so we use strings to map TAC code to MIPS
(defparameter *tac-to-mips* 
  '(
    (MIPS_MULT "mul.s")                 ;; $f0 := $f2 * $f4
    (MIPS_DIV "div.s")                  ;; $f0 := $f2 / $f4
    (MIPS_ADD "add.s")                  ;; $f0 := $f2 + $f4
    (MIPS_SUB "sub.s")                  ;; $f0 := $f2 - $f4
    (MIPS_UMINUS "sub.s")               ;; $f0 := 0.0 -$f2
    (MIPS_EQUALS "c.eq.s")              ;; if $f2 == $f4 then code = 1 else code = 0
    (MIPS_LESS_THAN_OR_EQUAL "c.le.s")  ;; if $f2 <= $f4 then code = 1 else code = 0
    (MIPS_LESS_THAN "c.lt.s")           ;; if $f2 < $f4 then code = 1 else code = 0
   ))

;; two functions to get type and value of tokens

;; in LALR parser, every token is a list whose first element is its type and second element its value.

;;; NOTE: the reason why i did not write any of these as macros is so that you can trace them if you feel like it

(defun t-get-type (x)
  "token type"
  (first x))

(defun t-get-val (x)
  "token value"
  (second x))

(defun wrap (x)
  "to wrap code in parentheses"
  (list x))

(defun pprint-code (code)
  (dolist (instruction (second code))
    (pprint instruction))
  t)

(defun mk-imm (register p)
  "MIPS does not allow mix arithmetic"
  (if (integerp p)
    (progn
      (format t "~%#converting to float")
      (format t "~%li $t0,~A" p)
      (format t "~%mtc1 $t0,$f6") ; using $f6 is temp value converter
      (format t "~%cvt.s.w ~A,$f6" register); MIPS internal conversion to float
      (format t "~%#conversion done"))
    (format t "~%li.s ~A,~A" register p)))  ; if your MIPS assembler has load immediate float command

(defun mk-mips (p register)
  "create li if constant or l.s if not"
  (if (numberp p)
    (mk-imm register p)  
    (format t "~%l.s ~A,~A" register p)))

(defun store-imm (temp p)
    (mk-imm "$f0" p)
    (format t "~%s.s $f0, ~A" temp))

(defun tac-get-mips (op)
  (second (assoc op *tac-to-mips*)))

(defun mk-mips-3ac (i)
  (let ((op (tac-get-mips (first i)))
        (p1 (second i))
        (p2 (third i))
        (p3 (fourth i)))
            (mk-mips (get-sym p2) "$f0")
            (mk-mips (get-sym p3) "$f2")
            (format t "~%~A $f0,$f0,$f2" op)
            (format t "~%s.s $f0,~A" (get-sym p1))))

(defun mk-mips-2ac (i)
  (let ((op (tac-get-mips (first i)))
        (p1 (second i))
        (p2 (third i)))
            (mk-mips (get-sym p2) "$f2")
            (format t "~%#loading dummy zero float to $f0")
            (format t "~%l.s $f0,zzeerroo")
            (format t "~%~A $f0,$f0,$f2" op)
            (format t "~%s.s $f0,~A" (get-sym p1))))

(defun mk-mips-2copy (i)
  (let ((p1 (first i))
        (p2 (second i)))
            (mk-mips (get-sym p2) "$f0")
            (format t "~%s.s $f0,~A" (get-sym p1))))

(defun mk-mips-boolean (i)
  (let ((op (tac-get-mips (first i))) ;; op -> `equals`, `less than` or `less than equals`
        (p1 (second i))
        (p2 (third i))
        (p3 (fourth i))
        (lblno (incf *labelno*)))
            (mk-mips (get-sym p2) "$f0")
            (mk-mips (get-sym p3) "$f2")
            (format t "~%#comparing two expressions, $f0, $f2 comparison result is stored in ~A as float (0.0/1.0)" (get-sym p1))
            (format t "~%~A $f0,$f2 # BOOL CALCULATION [~A]" op lblno)
            (format t "~%bc1f label_boolcalc_else_~d" lblno)
            (store-imm (get-sym p1) 1)
            (format t "~%j label_boolcalc_end_~d" lblno)
            (format t "~%label_boolcalc_else_~d:" lblno)
            (store-imm (get-sym p1) 0)
            (format t "~%label_boolcalc_end_~d: # BOOL CALCULATION DONE" lblno)))

(defun mk-mips-branch (i)
  (let ((p (first i)) ;; 1.0 or 0.0
        (pcode-true (second i))
        (pcode-false (third i))
        (lblno (incf *labelno*)))
            (format t "~%l.s $f0,~A #branch stmt [~A]" (get-sym p) lblno)
            (format t "~%l.s $f2,zzeerroo")
            (format t "~%c.eq.s $f0,$f2")
            (format t "~%bc1t label_branch_false_~d" lblno)
            (format t "~%#code to be executed if condition is true")
            (create-partial-code-segment pcode-true)
            (format t "~%#jump to end of if")
            (format t "~%j label_branch_end_~d" lblno)
            (format t "~%label_branch_false_~d:" lblno)
            (format t "~%#code to be executed if condition is false")
            (create-partial-code-segment pcode-false)
            (format t "~%label_branch_end_~d:" lblno)
            (format t "~%#end of branch")))

(defun mk-mips-and (i)
  (let ((p (first i))
        (cp1 (second i))
        (cp2 (third i))
        (lblno (incf *labelno*)))
            (format t "~%l.s $f0,~A # AND CALCULATION [~A]" (get-sym cp1) lblno)
            (format t "~%l.s $f2,zzeerroo")
            (format t "~%c.eq.s $f0,$f2")
            (format t "~%bc1t label_and_false_~d" lblno)

            (format t "~%l.s $f0,~A" (get-sym cp2))
            (format t "~%l.s $f2,zzeerroo")
            (format t "~%c.eq.s $f0,$f2")
            (format t "~%bc1t label_and_false_~d" lblno)
            
            (store-imm (get-sym p) 1)
            
            (format t "~%j label_and_end_~d" lblno)
            (format t "~%label_and_false_~d:" lblno)
            (store-imm (get-sym p) 0)
            (format t "~%label_and_end_~d:" lblno)))

(defun mk-mips-or (i)
  (let ((p (first i))
        (cp1 (second i))
        (cp2 (third i))
        (lblno (incf *labelno*)))
            (format t "~%l.s $f0,~A # OR CALCULATION [~A]" (get-sym cp1) lblno)
            (format t "~%l.s $f2,zzeerroo")
            (format t "~%c.eq.s $f0,$f2")
            (format t "~%bc1f label_or_true_~d" lblno)

	        (format t "~%l.s $f0,~A" (get-sym cp2))
            (format t "~%l.s $f2,zzeerroo")
            (format t "~%c.eq.s $f0,$f2")
            (format t "~%bc1f label_or_true_~d" lblno)

            (store-imm (get-sym p) 0)

            (format t "~%j label_or_end_~d" lblno)
            (format t "~%label_or_true_~d:" lblno)
            (store-imm (get-sym p) 1)
            (format t "~%label_or_end_~d:" lblno)))

(defun mk-mips-not (i)
  (let ((p (first i))
        (cp (second i))
        (lblno (incf *labelno*)))
            (format t "~%l.s $f0,~A # NOT CALCULATION [~A]" (get-sym cp) lblno)
            (format t "~%l.s $f2,zzeerroo")
            (format t "~%c.eq.s $f0,$f2")
            (format t "~%bc1t label_not_false_~d" lblno)

            (store-imm (get-sym p) 0)

            (format t "~%j label_not_end_~d" lblno)
            (format t "~%label_not_false_~d:" lblno)
            
            (store-imm (get-sym p) 1)
            
            (format t "~%label_not_end_~d:" lblno)))

(defun mk-mips-while (i)
  (let ((condp (first i))
        (cond (second i))
        (stmt (third i))
        (lblno (incf *labelno*)))
            (format t "~%label_while_start_~d: #label_while_start_~d" lblno lblno)
            (format t "~%l.s $f0,~A" (get-sym condp))
            (format t "~%l.s $f2,zzeerroo")
            (format t "~%c.eq.s $f0,$f2")
            (format t "~%bc1t label_while_end_~d" lblno)
            (format t "~%#code to be executed if condition is true in while")
            (create-partial-code-segment stmt)
            (create-partial-code-segment cond)
            (format t "~%j label_while_start_~d" lblno)
            (format t "~%label_while_end_~d:" lblno)
            (format t "~%#end of while")))

(defun get-arg-sym (ID)
    (get-sym (var-get-place ID)))

(defun pass-arg-by-value (reg arg)
  (mk-mips (get-arg-sym arg) reg))
  
(defun store-args-to-sp (args)
    (do ((i 0 (1+ i)))
        ((= i (length args)))
        (progn
            (pass-arg-by-value "$f0" (nth i args))
            (format t "~%s.s $f0, ~d($sp)" (* i 4)))))

(defun load-args-from-fp (args)
    (do ((i 0 (1+ i)))
        ((= i (length args)))
        (progn
            (format t "~%l.s $f0, ~d($fp)" (* i 4))
            (format t "~%s.s $f0, ~A" (get-arg-sym (nth i args))))))

(defun store-locals-to-sp (locals)
    (format t "~%addi $sp, $sp, -~d" (* 4 (length locals)))
    (do ((i 0 (1+ i)))
        ((= i (length locals)))
        (progn
            (format t "~%l.s $f0, ~A" (nth i locals))
            (format t "~%s.s $f0, ~d($sp)" (* i 4)))))

(defun load-locals-from-sp (locals)
    (do ((i 0 (1+ i)))
        ((= i (length locals)))
        (progn
            (format t "~%l.s $f0, ~d($sp)" (* i 4))
            (format t "~%s.s $f0, ~A" (nth i locals))))
    (format t "~%addi $sp, $sp, ~d" (* 4 (length locals))))

(defun prepare-locals (blockno)
    (setf *localvariables* '())
    (maphash #'(lambda (key val)
                (if (equal (second key) blockno)
                    (setf *localvariables* (append (list (second val)) *localvariables*))))
             *symtab*))

(defun has-procedure-call (stmts)
    (remove nil 
        (mapcar #'(lambda (instruction)
                    (cond 
                        ((equal (first instruction) 'procedure-call) t)
                        ((or (equal (first instruction) 'branch) (equal (first instruction) 'while))
                            (or (has-procedure-call (third instruction)) (has-procedure-call (fourth instruction))))
                        (t nil)))
                (second stmts))))

(defun mk-mips-procedure-def (i)
  (let* ((name (first i))
         (args (second i))
         (firstarg (first args))
	     (secondarg (second args))
	     (restargs (cddr args))
         (stmts (third i))
         (isleaf (not (has-procedure-call stmts)))
         (lblno (incf *labelno*)))
            (format t "~%j procedure_end_~d" lblno)
            (format t "~%~A: #procedure start [~A]" name name)
            
            (format t "~%addi $sp, $sp, -8")
            (format t "~%sw $ra, 0($sp)") ; store return address
            (format t "~%sw $fp, 4($sp)") ; store frame pointer
            (format t "~%addi $fp, $sp, 8") ; new AR

            (setf *blockno* (gethash name *funtab*))
            
            (if (not isleaf)
                (progn
                    (prepare-locals *blockno*) ;; updates *localvariables*
                    (store-locals-to-sp *localvariables*)))

            (if firstarg (format t "~%s.s $f12, ~A" (get-arg-sym firstarg)))
            (if secondarg (format t "~%s.s $f14, ~A" (get-arg-sym secondarg)))
	        (if restargs (load-args-from-fp restargs))
            
            (create-partial-code-segment stmts)
            (format t "~%li.s $f0, 0.0") ; default return value is 0.0
            (format t "~%mfc1 $v0, $f0")
            (format t "~%procedure_return_~d: #return label of procedure [~A]" *blockno* name)

            (if (not isleaf)
                (progn
                    (load-locals-from-sp *localvariables*)))
            
            (format t "~%lw $fp, 4($sp)") ; load frame pointer
            (format t "~%lw $ra, 0($sp)") ; load return address
            (format t "~%addi $sp, $sp, 8")
            (format t "~%jr $ra")
            (setf *blockno* 0)
            (format t "~%procedure_end_~d: #end of procedure [~A]" lblno name)))

(defun mk-mips-procedure-call (i)
  (let ((p (first i))
        (name (second i))
        (firstarg (first (third i)))
	    (secondarg (second (third i)))
	    (restargs (cddr (third i))))

	        (dolist (arg (third i))
	            (create-partial-code-segment (mk-code (var-get-code arg)))) ;; there may be expression in actual arguments

            (if firstarg (pass-arg-by-value "$f12" firstarg))
    	    (if secondarg (pass-arg-by-value "$f14" secondarg))
            (if restargs
                (progn
                    (format t "~%addi $sp, $sp, -~d" (* 4 (length restargs)))
                    ;; store all args to stack
                    (store-args-to-sp restargs)))
            (format t "~%jal ~A" name)
            (format t "~%mtc1 $v0, $f0")
            (if restargs
                (format t "~%addi $sp, $sp, ~d" (* 4 (length restargs)))) ;; clear stack
            (format t "~%s.s $f0, ~A" (get-sym p))))

(defun mk-mips-return (i)
  (let ((p (first i)))
           (mk-mips (get-sym p) "$f0")
           (format t "~%mfc1 $v0, $f0")
           (format t "~%j procedure_return_~d" *blockno*)))
           
(defun mk-mips-input (i)
  (let ((p (first i)))
           (format t "~%li $v0, 6")
           (format t "~%syscall")
           (format t "~%s.s $f0 ~A" (get-sym p))))

(defun mk-mips-output (i)
  (let ((p (first i)))
           (mk-mips (get-sym p) "$f12")
           (format t "~%li $v0, 2")
           (format t "~%syscall")))

(defun create-data-segment ()
  "only for variables; numbers will use immmediate loading rather than lw or l.s."
  (format t "~2%.data~%")
  (maphash #'(lambda (key val)
           (declare (ignore key))
           (if (equal (sym-get-type val) 'VAR)
               (format t "~%~A: .float 0.0" (sym-get-value val))))
       *symtab*)
  (format t "~%zzeerroo: .float 0.0")) ; MIPS has no float point zero constant like $zero for ints. bad bad

(defun create-partial-code-segment (code)
  (dolist (instruction (second code)) ; NB. code is a grammar variable feature (code (i1 i2 i3))
    (let ((itype (first instruction)))
      (cond 
        ((equal itype '3AC) (mk-mips-3ac (rest instruction)))
        ((equal itype '2AC) (mk-mips-2ac (rest instruction)))
        ((equal itype '2COPY) (mk-mips-2copy (rest instruction)))
        ((equal itype 'boolean) (mk-mips-boolean (rest instruction)))
        ((equal itype 'branch) (mk-mips-branch (rest instruction)))
        ((equal itype 'and) (mk-mips-and (rest instruction)))
        ((equal itype 'or) (mk-mips-or (rest instruction)))
        ((equal itype 'not) (mk-mips-not (rest instruction)))
        ((equal itype 'while) (mk-mips-while (rest instruction)))
        ((equal itype 'procedure-def) (mk-mips-procedure-def (rest instruction)))
        ((equal itype 'procedure-call) (mk-mips-procedure-call (rest instruction)))
        ((equal itype 'return) (mk-mips-return (rest instruction)))
        ((equal itype 'input) (mk-mips-input (rest instruction)))
        ((equal itype 'output) (mk-mips-output (rest instruction)))
        (t (format t "unknown TAC code: ~A" instruction))))))
        
(defun create-code-segment (code)
  (format t "~2%.text~2%")
  (format t "~2%.globl main~%")
  (format t "main:")
  (create-partial-code-segment code)
  (format t "~%procedure_return_0:") ; link to termination for return values
  (format t "~%#MIPs termination protocol:")
  (format t "~%li $v0,10") ; MIPs termination protocol
  (format t "~%syscall")
  (format t "~%.end main"))

(defun map-to-mips (code)
  (if (stringp *outstream*) (dribble *outstream*))  ; open out
  (create-data-segment) ; uses the symbol table
  (create-code-segment code)
  (if (stringp *outstream*) (dribble))) ; must close dribble

(defun tac-to-rac (code)
  (format t "~2%Symbol table at IC level:~2%key         value~%(name blockno)  (type value)~%--------------------")
  (maphash #'(lambda (key val)(format t "~%~A : ~A" key val)) *symtab*)
  (format t  "~2%TAC IC code:~%")
  (pprint-code code)
  (format t "~2%QtSpim target code:")
  (map-to-mips code))

;; some aux functions  to retrieve amd make feature values for grammar variables

(defun mk-place (v)
  (list 'place v))

(defun mk-code (v)
  (list 'code v))

(defun var-get-place (v)
  (second (assoc 'place v)))

(defun var-get-code (v)
  (second (assoc 'code v)))

(defun mk-2ac (op p1 p2)
  (wrap (list '2ac op p1 p2)))

(defun mk-3ac (op p1 p2 p3)
  (wrap (list '3ac op p1 p2 p3)))

(defun mk-2copy (p1 p2)
  (wrap (list '2copy p1 p2)))
  
(defun mk-boolean (op p1 p2 p3)
  (wrap (list 'boolean op p1 p2 p3)))

(defun mk-branch (cond p1 p2)
  (wrap (list 'branch cond p1 p2)))
  
(defun mk-and (p cond1 cond2)
  (wrap (list 'and p cond1 cond2)))
 
(defun mk-or (p cond1 cond2)
  (wrap (list 'or p cond1 cond2)))

(defun mk-not (p1 p2)
  (wrap (list 'not p1 p2)))

(defun mk-while (cp c s)
  (wrap (list 'while cp c s)))

(defun mk-procedure-def (name plist s)
  (wrap (list 'procedure-def name plist s)))

(defun mk-procedure-call (p name plist)
  (wrap (list 'procedure-call p name plist)))

(defun mk-return (p)
  (wrap (list 'return p)))

(defun mk-input (p)
  (wrap (list 'input p)))

(defun mk-output (p)
  (wrap (list 'output p)))

(defun newtemp ()
  (gensym "t"))       ; returns a new symbol prefixed t at Lisp run-time

;;;; LALR data 

(defparameter grammar
'(
  (start --> statements
    #'(lambda (statements)
      (tac-to-rac (mk-code (var-get-code statements)))))

  (statements --> statements entry SEPARATOR
    #'(lambda (statements entry SEPARATOR)
      (declare (ignore SEPARATOR))
      (list (mk-place nil)
            (mk-code (append (var-get-code statements)
                             (var-get-code entry))))))

  (statements --> #'(lambda () '()))

  (entry --> statement #'(lambda (statement) (identity statement)))

  (entry --> fdef #'(lambda (fdef) (identity fdef)))

  (statement --> assignment #'(lambda (assignment) (identity assignment)))

  (statement --> ifst #'(lambda (ifst) (identity ifst)))
 
  (statement --> whilest #'(lambda (whilest) (identity whilest)))

  (statement --> returnst #'(lambda (returnst) (identity returnst)))

  (statement --> io #'(lambda (io) (identity io)))

  (ifst --> IF_KW condition statements ifstTail END_IF_KW
    #'(lambda (IF_KW condition statements ifstTail END_IF_KW) 
      (declare (ignore IF_KW))
      (declare (ignore END_IF_KW))
      (list (mk-place nil)
            (mk-code (append (var-get-code condition)
                             (mk-branch (var-get-place condition)
                                        (mk-code (var-get-code statements))
                                        (mk-code (var-get-code ifstTail))))))))

  (ifstTail --> ELSE_KW statements
    #'(lambda (ELSE_KW statements) 
      (declare (ignore ELSE_KW))
      (identity statements)))

  (ifstTail --> #'(lambda () '()))

  (whilest --> WHILE_KW condition statements END_WHILE_KW
    #'(lambda (WHILE_KW condition statements END_WHILE_KW) 
      (declare (ignore WHILE_KW))
      (declare (ignore END_WHILE_KW))
      (list (mk-place nil)
            (mk-code (append (var-get-code condition)
                             (mk-while (var-get-place condition)
                                      (mk-code (var-get-code condition))
                                      (mk-code (var-get-code statements))))))))

  (returnst --> RET_KW xpr
    #'(lambda (RET_KW xpr) 
      (declare (ignore RET_KW))
      (list (mk-place nil)
            (mk-code (append (var-get-code xpr)
                             (mk-return (var-get-place xpr)))))))

  (io --> INPUT_KW ID
    #'(lambda (INPUT_KW ID) 
      (declare (ignore INPUT_KW))
      (mk-sym-entry (t-get-val ID))
      (list (mk-place nil)
            (mk-code (mk-input (t-get-val ID))))))

  (io --> OUTPUT_KW ID
    #'(lambda (OUTPUT_KW ID) 
      (declare (ignore OUTPUT_KW))
      (list (mk-place nil)
            (mk-code (mk-output (t-get-val ID))))))

  (fdef --> FUN_KW ID plist statements END_FUN_KW
    #'(lambda (FUN_KW ID plist statements END_FUN_KW) 
      (declare (ignore FUN_KW))
      (declare (ignore END_FUN_KW))
      (setf (gethash (t-get-val ID) *funtab*) *blockno*) ; store block no in the table
      (setf *blockno* 0) ; reset blockno
      (list (mk-place nil)
            (mk-code (mk-procedure-def (t-get-val ID)
                                       plist 
                                       (mk-code (var-get-code statements)))))))

  (plist --> P_L_START plistp1 P_L_END
    #'(lambda (P_L_START plistp1 P_L_END) 
      (declare (ignore P_L_START))
      (declare (ignore P_L_END))
      (incf *maxblockno*) ; block enter
      (setf *blockno* *maxblockno*) ; increase block no, will be decreased at `fdef` rule
      (identity plistp1)))

  (plistp1 --> plistp1 P_L_SEP ID
    #'(lambda (plistp1 P_L_SEP ID) 
      (declare (ignore P_L_SEP))
      (mk-sym-entry-to-next-block (t-get-val ID))
      (append plistp1 
              (list (list (mk-place (t-get-val ID))
                          (mk-code nil))))))

  (plistp1 --> ID
    #'(lambda (ID) ;; This is formal argument of function
      (mk-sym-entry-to-next-block (t-get-val ID))
      (list (list (mk-place (t-get-val ID))
                  (mk-code nil)))))
  
  (plistp1 --> #'(lambda () NIL))

  (fplist --> P_L_START fplistp1 P_L_END
    #'(lambda (P_L_START fplistp1 P_L_END) 
      (declare (ignore P_L_START))
      (declare (ignore P_L_END))
      (identity fplistp1)))

  (fplistp1 --> fplistp1 P_L_SEP xpr
    #'(lambda (fplistp1 P_L_SEP xpr) 
      (declare (ignore P_L_SEP))
      (append fplistp1 (list (identity xpr)))))

  (fplistp1 --> xpr
    #'(lambda (xpr)
      (list (identity xpr))))

  (fplistp1 --> #'(lambda () NIL))

  (assignment --> ID ASM_OP xpr
    #'(lambda (ID ASM_OP xpr)
      (declare (ignore ASM_OP)) 
      (progn
            (mk-sym-entry (t-get-val ID))
            (list (mk-place (t-get-val ID))
                  (mk-code (append (var-get-code xpr)
                                   (mk-2copy (t-get-val ID) (var-get-place xpr))))))))

  (xpr --> xpr ADD xprp
    #'(lambda (xpr ADD xprp)
      (declare (ignore ADD))
        (let ((newplace (newtemp)))
            (mk-sym-entry newplace)
            (list (mk-place newplace)
                  (mk-code (append (var-get-code xpr)
                                   (var-get-code xprp)
                                   (mk-3ac 'MIPS_ADD newplace
                                       (var-get-place xpr)
                                       (var-get-place xprp))))))))

  (xpr --> xpr SUB xprp
    #'(lambda (xpr SUB xprp) 
      (declare (ignore SUB)) 
      (let ((newplace (newtemp)))
         (mk-sym-entry newplace)
         (list (mk-place newplace)
               (mk-code (append (var-get-code xpr)
                                (var-get-code xprp)
                                (mk-3ac 'MIPS_SUB newplace
                                    (var-get-place xpr)
                                    (var-get-place xprp))))))))

  (xpr --> xprp #'(lambda (xprp) (identity xprp)))

  (xprp --> xprp MULT arg
    #'(lambda (xprp MULT arg)
       (declare (ignore MULT))
       (let ((newplace (newtemp)))
         (mk-sym-entry newplace)
         (list (mk-place newplace)
               (mk-code (append (var-get-code xprp)
                                (var-get-code arg)
                                (mk-3ac 'MIPS_MULT newplace
                                    (var-get-place xprp)
                                    (var-get-place arg))))))))
  (xprp --> xprp DIV arg
    #'(lambda (xprp DIV arg) 
      (declare (ignore DIV))
      (let ((newplace (newtemp)))
         (mk-sym-entry newplace)
         (list (mk-place newplace)
               (mk-code (append (var-get-code xprp)
                                (var-get-code arg)
                                (mk-3ac 'MIPS_DIV newplace
                                    (var-get-place xprp)
                                    (var-get-place arg))))))))

  (xprp --> arg #'(lambda (arg)(identity arg)))

  (arg --> ID
    #'(lambda (ID)
      (mk-sym-entry-if-constant (t-get-val ID))
      (list (mk-place (t-get-val ID))
            (mk-code nil))))
  
  (arg --> SUB ID
    #'(lambda (SUB ID)
      (declare (ignore SUB))
      (let ((newplace (newtemp)))
           (mk-sym-entry newplace)
           (mk-sym-entry (t-get-val ID))
           (list (mk-place newplace)
                 (mk-code (mk-2ac 'MIPS_UMINUS newplace (t-get-val ID)))))))

  (arg --> P_L_START xpr P_L_END
    #'(lambda (P_L_START xpr P_L_END)
      (declare (ignore P_L_START))
      (declare (ignore P_L_END))
      (identity xpr)))

  (arg --> ID fplist
    #'(lambda (ID fplist) 
      (let ((newplace (newtemp)))
           (mk-sym-entry newplace)
           (list (mk-place newplace)
                 (mk-code (mk-procedure-call newplace (t-get-val ID) fplist))))))

  (condition --> U_OP lcondition
    #'(lambda (U_OP lcondition) 
      (declare (ignore U_OP))
      (let ((newplace (newtemp)))
           (mk-sym-entry newplace)
           (list (mk-place newplace)
                 (mk-code (append (var-get-code lcondition)
                                  (mk-not newplace
                                    (var-get-place lcondition))))))))

  (condition --> lcondition #'(lambda (lcondition)(identity lcondition)))

  (lcondition --> bcondition #'(lambda (bcondition)(identity bcondition)))

  (lcondition --> bcondition AND_KW condition
    #'(lambda (bcondition AND_KW condition) 
      (declare (ignore AND_KW))
      (let ((newplace (newtemp)))
           (mk-sym-entry newplace)
           (list (mk-place newplace)
                 (mk-code (append (var-get-code bcondition)
                                  (var-get-code condition)
                                  (mk-and newplace
                                    (var-get-place bcondition)
                                    (var-get-place condition))))))))

  (lcondition --> bcondition OR_KW condition
    #'(lambda (bcondition OR_KW condition) 
      (declare (ignore OR_KW))
      (let ((newplace (newtemp)))
           (mk-sym-entry newplace)
           (list (mk-place newplace)
                 (mk-code (append (var-get-code bcondition)
                                  (var-get-code condition)
                                  (mk-or newplace
                                    (var-get-place bcondition)
                                    (var-get-place condition))))))))

  (bcondition --> xpr LESS_THAN xpr
    #'(lambda (xpr LESS_THAN xpr2)
      (declare (ignore LESS_THAN))
      (let ((newplace (newtemp)))
           (mk-sym-entry newplace)
           (list (mk-place newplace)
                 (mk-code (append (var-get-code xpr)
                                  (var-get-code xpr2)
                                  (mk-boolean 'MIPS_LESS_THAN
                                              newplace
                                              (var-get-place xpr)
                                              (var-get-place xpr2))))))))

  (bcondition --> xpr LESS_THAN_OR_EQUALS_TO xpr
    #'(lambda (xpr LESS_THAN_OR_EQUALS_TO xpr2)
      (declare (ignore LESS_THAN_OR_EQUALS_TO))
      (let ((newplace (newtemp)))
           (mk-sym-entry newplace)
           (list (mk-place newplace)
                 (mk-code (append (var-get-code xpr)
                                  (var-get-code xpr2)
                                  (mk-boolean 'MIPS_LESS_THAN_OR_EQUAL
                                              newplace
                                              (var-get-place xpr)
                                              (var-get-place xpr2))))))))

  (bcondition  --> xpr EQUALS xpr
    #'(lambda (xpr EQUALS xpr2)
      (declare (ignore EQUALS))
      (let ((newplace (newtemp)))
           (mk-sym-entry newplace)
           (list (mk-place newplace)
                 (mk-code (append (var-get-code xpr)
                                  (var-get-code xpr2)
                                  (mk-boolean 'MIPS_EQUALS
                                              newplace
                                              (var-get-place xpr)
                                              (var-get-place xpr2))))))))

  (bcondition  --> xpr GREATER_THAN xpr
    #'(lambda (xpr GREATER_THAN xpr2)
      (declare (ignore GREATER_THAN))
      (let ((newplace (newtemp)))
           (mk-sym-entry newplace)
           (list (mk-place newplace)
                 (mk-code (append (var-get-code xpr2)
                                  (var-get-code xpr)
                                  (mk-boolean 'MIPS_LESS_THAN
                                              newplace
                                              (var-get-place xpr2)
                                              (var-get-place xpr))))))))
    
  (bcondition  --> xpr GREATER_THAN_OR_EQUALS_TO xpr
    #'(lambda (xpr GREATER_THAN_OR_EQUALS_TO xpr2)
      (declare (ignore GREATER_THAN_OR_EQUALS_TO))
      (let ((newplace (newtemp)))
           (mk-sym-entry newplace)
           (list (mk-place newplace)
                 (mk-code (append (var-get-code xpr2)
                                  (var-get-code xpr)
                                  (mk-boolean 'MIPS_LESS_THAN_OR_EQUAL
                                              newplace
                                              (var-get-place xpr2)
                                              (var-get-place xpr))))))))

  ))

(defparameter lexforms '(ID SEPARATOR RET_KW IF_KW ELSE_KW END_IF_KW WHILE_KW END_WHILE_KW FUN_KW END_FUN_KW INPUT_KW OUTPUT_KW AND_KW OR_KW U_OP ASM_OP P_L_START P_L_END ADD SUB MULT DIV LESS_THAN LESS_THAN_OR_EQUALS_TO EQUALS GREATER_THAN GREATER_THAN_OR_EQUALS_TO P_L_SEP))

(defparameter lexicon
    '(
        (\;        SEPARATOR) ;; all but ID goes in the lexicon
        (|return|  RET_KW)
        (|if|      IF_KW)
        (|else|    ELSE_KW)
        (|endi|    END_IF_KW)
        (|while|   WHILE_KW)
        (|endw|    END_WHILE_KW)
        (|fun|     FUN_KW)
        (|endf|    END_FUN_KW)
        (|input|   INPUT_KW)
        (|output|  OUTPUT_KW)
        (|and|     AND_KW)
        (|or|      OR_KW)
        (|!|       U_OP)
        (|+|       ADD)
        (|-|       SUB)
        (|*|       MULT)
        (|/|       DIV)
        (|:=|      ASM_OP)
        (|<|       LESS_THAN)
        (|<=|      LESS_THAN_OR_EQUALS_TO)
        (|==|      EQUALS)
        (|>|       GREATER_THAN)
        (|>=|      GREATER_THAN_OR_EQUALS_TO)
        (|(|       P_L_START)
        (|)|       P_L_END)
        (|,|       P_L_SEP)
        (|$|       $)))        ; this is for lalrparser.lisp's end of input

  ;; if you change the end-marker, change its hardcopy above in lexicon above as well.
  ;; (because LALR parser does not evaluate its lexicon symbols---sorry.)
(defparameter *ENDMARKER* '$)

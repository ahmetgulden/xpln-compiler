

.data

PARAM1_1: .float 0.0
PARAM2_1: .float 0.0
t635_1: .float 0.0
t636_1: .float 0.0
t637_1: .float 0.0
t638_1: .float 0.0
t639_1: .float 0.0
t640_1: .float 0.0
t641_1: .float 0.0
t642_0: .float 0.0
RESULT_0: .float 0.0
zzeerroo: .float 0.0

.text



.globl main
main:
j procedure_end_1
FOO: #procedure start [FOO]
addi $sp, $sp, -8
sw $ra, 0($sp)
sw $fp, 4($sp)
addi $fp, $sp, 8
s.s $f12, PARAM1_1
s.s $f14, PARAM2_1
l.s $f0,PARAM1_1
#converting to float
li $t0,4
mtc1 $t0,$f6
cvt.s.w $f2,$f6
#conversion done
#comparing two expressions, $f0, $f2 comparison result is stored in t635_1 as float (0.0/1.0)
c.lt.s $f0,$f2 # BOOL CALCULATION [2]
bc1f label_boolcalc_else_2
#converting to float
li $t0,1
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t635_1
j label_boolcalc_end_2
label_boolcalc_else_2:
#converting to float
li $t0,0
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t635_1
label_boolcalc_end_2: # BOOL CALCULATION DONE
#converting to float
li $t0,5
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
l.s $f2,PARAM2_1
#comparing two expressions, $f0, $f2 comparison result is stored in t636_1 as float (0.0/1.0)
c.lt.s $f0,$f2 # BOOL CALCULATION [3]
bc1f label_boolcalc_else_3
#converting to float
li $t0,1
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t636_1
j label_boolcalc_end_3
label_boolcalc_else_3:
#converting to float
li $t0,0
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t636_1
label_boolcalc_end_3: # BOOL CALCULATION DONE
l.s $f0,PARAM1_1
l.s $f2,PARAM2_1
#comparing two expressions, $f0, $f2 comparison result is stored in t637_1 as float (0.0/1.0)
c.eq.s $f0,$f2 # BOOL CALCULATION [4]
bc1f label_boolcalc_else_4
#converting to float
li $t0,1
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t637_1
j label_boolcalc_end_4
label_boolcalc_else_4:
#converting to float
li $t0,0
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t637_1
label_boolcalc_end_4: # BOOL CALCULATION DONE
l.s $f0,t637_1 # NOT CALCULATION [5]
l.s $f2,zzeerroo
c.eq.s $f0,$f2
bc1t label_not_false_5
#converting to float
li $t0,0
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t638_1
j label_not_end_5
label_not_false_5:
#converting to float
li $t0,1
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t638_1
label_not_end_5:
l.s $f0,t636_1 # AND CALCULATION [6]
l.s $f2,zzeerroo
c.eq.s $f0,$f2
bc1t label_and_false_6
l.s $f0,t638_1
l.s $f2,zzeerroo
c.eq.s $f0,$f2
bc1t label_and_false_6
#converting to float
li $t0,1
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t639_1
j label_and_end_6
label_and_false_6:
#converting to float
li $t0,0
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t639_1
label_and_end_6:
l.s $f0,t635_1 # AND CALCULATION [7]
l.s $f2,zzeerroo
c.eq.s $f0,$f2
bc1t label_and_false_7
l.s $f0,t639_1
l.s $f2,zzeerroo
c.eq.s $f0,$f2
bc1t label_and_false_7
#converting to float
li $t0,1
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t640_1
j label_and_end_7
label_and_false_7:
#converting to float
li $t0,0
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t640_1
label_and_end_7:
l.s $f0,t640_1 #branch stmt [8]
l.s $f2,zzeerroo
c.eq.s $f0,$f2
bc1t label_branch_false_8
#code to be executed if condition is true
#converting to float
li $t0,0
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
mfc1 $v0, $f0
j procedure_return_1
#jump to end of if
j label_branch_end_8
label_branch_false_8:
#code to be executed if condition is false
l.s $f0,PARAM1_1
l.s $f2,PARAM2_1
div.s $f0,$f0,$f2
s.s $f0,t641_1
l.s $f0,t641_1
mfc1 $v0, $f0
j procedure_return_1
label_branch_end_8:
#end of branch
li.s $f0, 0.0
mfc1 $v0, $f0
procedure_return_1: #return label of procedure [FOO]
lw $fp, 4($sp)
lw $ra, 0($sp)
addi $sp, $sp, 8
jr $ra
procedure_end_1: #end of procedure [FOO]
#converting to float
li $t0,3
mtc1 $t0,$f6
cvt.s.w $f12,$f6
#conversion done
#converting to float
li $t0,6
mtc1 $t0,$f6
cvt.s.w $f14,$f6
#conversion done
jal FOO
mtc1 $v0, $f0
s.s $f0, t642_0
l.s $f0,t642_0
s.s $f0,RESULT_0
l.s $f12,RESULT_0
li $v0, 2
syscall
procedure_return_0:
#MIPs termination protocol:
li $v0,10
syscall
.end main
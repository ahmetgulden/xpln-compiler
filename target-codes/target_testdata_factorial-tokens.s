

.data

N_1: .float 0.0
NUM_1: .float 0.0
t643_1: .float 0.0
t644_1: .float 0.0
t645_1: .float 0.0
t646_0: .float 0.0
RESULT_0: .float 0.0
zzeerroo: .float 0.0

.text



.globl main
main:
j procedure_end_1
FACTORIAL: #procedure start [FACTORIAL]
addi $sp, $sp, -8
sw $ra, 0($sp)
sw $fp, 4($sp)
addi $fp, $sp, 8
s.s $f12, N_1
#converting to float
li $t0,1
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0,NUM_1
#converting to float
li $t0,1
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
l.s $f2,N_1
#comparing two expressions, $f0, $f2 comparison result is stored in t643_1 as float (0.0/1.0)
c.le.s $f0,$f2 # BOOL CALCULATION [2]
bc1f label_boolcalc_else_2
#converting to float
li $t0,1
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t643_1
j label_boolcalc_end_2
label_boolcalc_else_2:
#converting to float
li $t0,0
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t643_1
label_boolcalc_end_2: # BOOL CALCULATION DONE
label_while_start_3: #label_while_start_3
l.s $f0,t643_1
l.s $f2,zzeerroo
c.eq.s $f0,$f2
bc1t label_while_end_3
#code to be executed if condition is true in while
l.s $f0,NUM_1
l.s $f2,N_1
mul.s $f0,$f0,$f2
s.s $f0,t644_1
l.s $f0,t644_1
s.s $f0,NUM_1
l.s $f0,N_1
#converting to float
li $t0,1
mtc1 $t0,$f6
cvt.s.w $f2,$f6
#conversion done
sub.s $f0,$f0,$f2
s.s $f0,t645_1
l.s $f0,t645_1
s.s $f0,N_1
#converting to float
li $t0,1
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
l.s $f2,N_1
#comparing two expressions, $f0, $f2 comparison result is stored in t643_1 as float (0.0/1.0)
c.le.s $f0,$f2 # BOOL CALCULATION [4]
bc1f label_boolcalc_else_4
#converting to float
li $t0,1
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t643_1
j label_boolcalc_end_4
label_boolcalc_else_4:
#converting to float
li $t0,0
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0, t643_1
label_boolcalc_end_4: # BOOL CALCULATION DONE
j label_while_start_3
label_while_end_3:
#end of while
l.s $f0,NUM_1
mfc1 $v0, $f0
j procedure_return_1
li.s $f0, 0.0
mfc1 $v0, $f0
procedure_return_1: #return label of procedure [FACTORIAL]
lw $fp, 4($sp)
lw $ra, 0($sp)
addi $sp, $sp, 8
jr $ra
procedure_end_1: #end of procedure [FACTORIAL]
#converting to float
li $t0,4
mtc1 $t0,$f6
cvt.s.w $f12,$f6
#conversion done
jal FACTORIAL
mtc1 $v0, $f0
s.s $f0, t646_0
l.s $f0,t646_0
s.s $f0,RESULT_0
l.s $f12,RESULT_0
li $v0, 2
syscall
procedure_return_0:
#MIPs termination protocol:
li $v0,10
syscall
.end main
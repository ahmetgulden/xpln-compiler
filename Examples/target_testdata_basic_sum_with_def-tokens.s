

.data

NUM1_0: .float 0.0
NUM2_0: .float 0.0
ARG1_1: .float 0.0
ARG2_1: .float 0.0
t633_1: .float 0.0
t634_0: .float 0.0
RESULT_0: .float 0.0
zzeerroo: .float 0.0

.text



.globl main
main:
#converting to float
li $t0,3
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0,NUM1_0
#converting to float
li $t0,5
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0,NUM2_0
j procedure_end_1
SUM: #procedure start [SUM]
addi $sp, $sp, -8
sw $ra, 0($sp)
sw $fp, 4($sp)
addi $fp, $sp, 8
s.s $f12, ARG1_1
s.s $f14, ARG2_1
l.s $f0,ARG1_1
l.s $f2,ARG2_1
add.s $f0,$f0,$f2
s.s $f0,t633_1
l.s $f0,t633_1
mfc1 $v0, $f0
j procedure_return_1
li.s $f0, 0.0
mfc1 $v0, $f0
procedure_return_1: #return label of procedure [SUM]
lw $fp, 4($sp)
lw $ra, 0($sp)
addi $sp, $sp, 8
jr $ra
procedure_end_1: #end of procedure [SUM]
l.s $f12,NUM1_0
l.s $f14,NUM2_0
jal SUM
mtc1 $v0, $f0
s.s $f0, t634_0
l.s $f0,t634_0
s.s $f0,RESULT_0
l.s $f12,RESULT_0
li $v0, 2
syscall
procedure_return_0:
#MIPs termination protocol:
li $v0,10
syscall
.end main
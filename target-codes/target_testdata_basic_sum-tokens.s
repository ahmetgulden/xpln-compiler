

.data

NUM1_0: .float 0.0
NUM2_0: .float 0.0
t632_0: .float 0.0
SUM_0: .float 0.0
zzeerroo: .float 0.0

.text



.globl main
main:
#converting to float
li $t0,1
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0,NUM1_0
#converting to float
li $t0,6
mtc1 $t0,$f6
cvt.s.w $f0,$f6
#conversion done
s.s $f0,NUM2_0
l.s $f0,NUM1_0
l.s $f2,NUM2_0
add.s $f0,$f0,$f2
s.s $f0,t632_0
l.s $f0,t632_0
s.s $f0,SUM_0
l.s $f12,SUM_0
li $v0, 2
syscall
procedure_return_0:
#MIPs termination protocol:
li $v0,10
syscall
.end main
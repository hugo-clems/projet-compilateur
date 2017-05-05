.class MyClass
.super java/lang/Object


.method static even(I)I
  .limit stack 6
  .limit locals 0
     iload 0
     sipush 0
     if_icmplt lab_2_0
     sipush 0
     goto lab_2_1
  lab_2_0:
     sipush 1
  lab_2_1:
     sipush 0
     if_icmpeq lab_0
     sipush 0
     iload 0
     isub
     istore 0
     goto lab_2
  lab_0:
     nop
  lab_2:
  lab_1:
     iload 0
     sipush 1
     if_icmpgt lab_2_0
     sipush 0
     goto lab_2_1
  lab_2_0:
     sipush 1
  lab_2_1:
     sipush 0
     if_icmpeq lab_0
     iload 0
     sipush 2
     isub
     istore 0
     goto lab_1
  lab_0:
     nop
     ireturn
.end method


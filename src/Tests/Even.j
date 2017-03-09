.class MyClass
.super java/lang/Object


.method static even(I)I
  .limit stack 3
  .limit locals 1
     sipush 10
     sipush 2
     irem
     ireturn
.end method

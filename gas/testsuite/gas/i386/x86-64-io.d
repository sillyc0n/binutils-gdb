#objdump: -dw
#name: x86-64 rex.W in/out

.*: +file format .*

Disassembly of section .text:

0+000 <_in>:
   0:	48 ed                	rex.W in \(%dx\),%eax
   2:	66 48 ed             	data16 rex.W in \(%dx\),%eax

0+005 <_out>:
   5:	48 ef                	rex.W out %eax,\(%dx\)
   7:	66 48 ef             	data16 rex.W out %eax,\(%dx\)

0+00a <_ins>:
   a:	48 6d                	rex.W insl \(%dx\),\(%rdi\)
   c:	66 48 6d             	data16 rex.W insl \(%dx\),\(%rdi\)

0+00f <_outs>:
   f:	48 6f                	rex.W outsl \(%rsi\),\(%dx\)
  11:	66 48 6f             	data16 rex.W outsl \(%rsi\),\(%dx\)
#pass

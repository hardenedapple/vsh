vshcmd: > gcc -g3 tree.c -o tree
demo [09:47:11] $ 
vshcmd: > gdb ./tree
vshcmd: > source ~/.vim/bundle/vsh/integration/with_gdb.py
Reading symbols from ./tree...
(gdb) (gdb) 
vshcmd: > start 10
vshcmd: > showhere

vshcmd: > break free_tree
vshcmd: > cont
Breakpoint 2 at 0x55555555534d: file tree.c, line 54.
(gdb) Continuing.

Breakpoint 2, free_tree (root=0x5555555592a0) at tree.c:54
54	    if (root) {
(gdb) 
vshcmd: > showhere
(gdb) 
vshcmd: > next
vshcmd: > gohere
55	        free_tree(root->children[larger]);
(gdb) (gdb) 
vshcmd: > bt
#0  free_tree (root=0x5555555592a0) at tree.c:55
#1  0x00005555555554be in main (argc=2, argv=0x7fffffffe0d8) at tree.c:93
(gdb) 
vshcmd: > # Below is not part of this plugin (just a small function I have locally)
vshcmd: > # vimcmd: delmarks A B
vshcmd: > mark-stack
(gdb) 
vshcmd: > quit
vshcmd: > y
A debugging session is active.

	Inferior 1 [process 1244087] will be killed.

Quit anyway? (y or n) demo [09:51:09] $ 

vshcmd: > # We can munge the output of commands after they have been run with
vshcmd: > # all the power of vim.
vshcmd: > # i.e. you realise after printing something out that you should have
vshcmd: > # piped it through grep.
vshcmd: > # Along with the text objects described above, there is a shortcut to
vshcmd: > # enter the range in the command line, so '<localleader>od<CR>' would
vshcmd: > # do the same as 'dio'.
vshcmd: > # cat ../autoload/vsh/vsh_shell_start
vshcmd: > # <localleader>ov/export/d<CR>
vshcmd: > # dio
vshcmd: > cat ../autoload/vsh/vsh_shell_start
vshcmd: > # Because this is directly connected to a pseudo terminal, we can
vshcmd: > # query bash for completions by sending the readline binding for
vshcmd: > # 'possible-completions' (by default this is done with the vim
vshcmd: > # shortcut '<localloader>l' in normal mode, or '<C-q>' in insert
vshcmd: > # mode.
vshcmd: > # read<C-q> will show completions for "read", and since this is a vim
vshcmd: > # buffer, you can then select the completions with plain vim completion
vshcmd: > # '<C-n>' and '<C-p>'
vshcmd: > 
vshcmd: > # You can easily run other programs like gdb
vshcmd: > cd ~/share/repos/neovim
neovim [14:11:11] $ 
vshcmd: > gdb build/bin/nvim
Reading symbols from build/bin/nvim...done.
(gdb) 
vshcmd: > # Though sometimes you need some config differences
vshcmd: > # (I have this in my gdb config predicated on $TERM='dumb' so this
vshcmd: > # happens automatically).
vshcmd: > set pagination off
(gdb) 
vshcmd: > # Being able to search through input is handy here too.
vshcmd: > # Seeing as it's just text in a vim buffer, you can even filter the
vshcmd: > # output through a shell command or with a vim command,
vshcmd: > # <localleader>o! grep call<CR>
vshcmd: > # u
vshcmd: > # <localleader>o v/cmp/d<CR>
vshcmd: > disassemble u_savecommon
Dump of assembler code for function u_savecommon:
   0x000000000064c63e <+0>:	push   %rbp
   0x000000000064c63f <+1>:	mov    %rsp,%rbp
   0x000000000064c642 <+4>:	push   %rbx
   0x000000000064c643 <+5>:	sub    $0x78,%rsp
   0x000000000064c647 <+9>:	mov    %rdi,-0x68(%rbp)
   0x000000000064c64b <+13>:	mov    %rsi,-0x70(%rbp)
   0x000000000064c64f <+17>:	mov    %rdx,-0x78(%rbp)
   0x000000000064c653 <+21>:	mov    %ecx,-0x7c(%rbp)
   0x000000000064c656 <+24>:	mov    %fs:0x28,%rax
   0x000000000064c65f <+33>:	mov    %rax,-0x18(%rbp)
   0x000000000064c663 <+37>:	xor    %eax,%eax
   0x000000000064c665 <+39>:	cmpl   $0x0,-0x7c(%rbp)
   0x000000000064c669 <+43>:	jne    0x64c6bc <u_savecommon+126>
   0x000000000064c66b <+45>:	callq  0x64c4f0 <undo_allowed>   # You can write notes in the debugging output.
   0x000000000064c670 <+50>:	xor    $0x1,%eax
   0x000000000064c673 <+53>:	test   %al,%al
   0x000000000064c675 <+55>:	je     0x64c681 <u_savecommon+67>
   0x000000000064c677 <+57>:	mov    $0x0,%eax
   0x000000000064c67c <+62>:	jmpq   0x64ceec <u_savecommon+2222>
   0x000000000064c681 <+67>:	mov    $0x0,%edi
   0x000000000064c686 <+72>:	callq  0x55c715 <change_warning>
   0x000000000064c68b <+77>:	mov    0x34b46e(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c692 <+84>:	mov    0x8(%rax),%rax
   0x000000000064c696 <+88>:	add    $0x1,%rax
   0x000000000064c69a <+92>:	cmp    -0x70(%rbp),%rax
   0x000000000064c69e <+96>:	jge    0x64c6bc <u_savecommon+126>
   0x000000000064c6a0 <+98>:	mov    $0x713ba0,%edi
   0x000000000064c6a5 <+103>:	callq  0x42e8e0 <gettext@plt>
   0x000000000064c6aa <+108>:	mov    %rax,%rdi
   0x000000000064c6ad <+111>:	callq  0x550718 <emsg>
   0x000000000064c6b2 <+116>:	mov    $0x0,%eax
   0x000000000064c6b7 <+121>:	jmpq   0x64ceec <u_savecommon+2222>
   0x000000000064c6bc <+126>:	mov    -0x70(%rbp),%rax
   0x000000000064c6c0 <+130>:	sub    -0x68(%rbp),%rax
   0x000000000064c6c4 <+134>:	sub    $0x1,%rax
   0x000000000064c6c8 <+138>:	mov    %rax,-0x20(%rbp)
   0x000000000064c6cc <+142>:	mov    0x34b42d(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c6d3 <+149>:	movzbl 0x1e04(%rax),%eax
   0x000000000064c6da <+156>:	test   %al,%al
   0x000000000064c6dc <+158>:	je     0x64cb42 <u_savecommon+1284>
   0x000000000064c6e2 <+164>:	mov    0x34b417(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c6e9 <+171>:	movb   $0x1,0x156c(%rax)
   0x000000000064c6f0 <+178>:	callq  0x64c57e <get_undolevel>
   0x000000000064c6f5 <+183>:	test   %rax,%rax
   0x000000000064c6f8 <+186>:	js     0x64c70a <u_savecommon+204>
   0x000000000064c6fa <+188>:	mov    $0x4a8,%edi
   0x000000000064c6ff <+193>:	callq  0x54bdd4 <xmalloc>
   0x000000000064c704 <+198>:	mov    %rax,-0x40(%rbp)             # Or more easily follow what's happening to an individual register.
   0x000000000064c708 <+202>:	jmp    0x64c712 <u_savecommon+212>
   0x000000000064c70a <+204>:	movq   $0x0,-0x40(%rbp)
   0x000000000064c712 <+212>:	mov    0x34b3e7(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c719 <+219>:	mov    0x1df8(%rax),%rax
   0x000000000064c720 <+226>:	mov    %rax,-0x58(%rbp)
   0x000000000064c724 <+230>:	mov    -0x58(%rbp),%rax
   0x000000000064c728 <+234>:	test   %rax,%rax
   0x000000000064c72b <+237>:	je     0x64c7f1 <u_savecommon+435>
   0x000000000064c731 <+243>:	mov    0x34b3c8(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c738 <+250>:	mov    -0x58(%rbp),%rdx
   0x000000000064c73c <+254>:	mov    (%rdx),%rdx
   0x000000000064c73f <+257>:	mov    %rdx,0x1df0(%rax)
   0x000000000064c746 <+264>:	mov    0x34b3b3(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c74d <+271>:	movq   $0x0,0x1df8(%rax)
   0x000000000064c758 <+282>:	jmpq   0x64c7f1 <u_savecommon+435>
   0x000000000064c75d <+287>:	mov    0x34b39c(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c764 <+294>:	mov    0x1de8(%rax),%rax
   0x000000000064c76b <+301>:	mov    %rax,-0x28(%rbp)
   0x000000000064c76f <+305>:	mov    -0x58(%rbp),%rax
   0x000000000064c773 <+309>:	cmp    %rax,-0x28(%rbp)
   0x000000000064c777 <+313>:	jne    0x64c795 <u_savecommon+343>
   0x000000000064c779 <+315>:	mov    0x34b380(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c780 <+322>:	lea    -0x58(%rbp),%rdx
   0x000000000064c784 <+326>:	mov    -0x28(%rbp),%rcx
   0x000000000064c788 <+330>:	mov    %rcx,%rsi
   0x000000000064c78b <+333>:	mov    %rax,%rdi
   0x000000000064c78e <+336>:	callq  0x6522f8 <u_freebranch>
   0x000000000064c793 <+341>:	jmp    0x64c7f1 <u_savecommon+435>
   0x000000000064c795 <+343>:	mov    -0x28(%rbp),%rax
   0x000000000064c799 <+347>:	mov    0x10(%rax),%rax
   0x000000000064c79d <+351>:	test   %rax,%rax
   0x000000000064c7a0 <+354>:	jne    0x64c7ca <u_savecommon+396>
   0x000000000064c7a2 <+356>:	mov    0x34b357(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c7a9 <+363>:	lea    -0x58(%rbp),%rdx
   0x000000000064c7ad <+367>:	mov    -0x28(%rbp),%rcx
   0x000000000064c7b1 <+371>:	mov    %rcx,%rsi
   0x000000000064c7b4 <+374>:	mov    %rax,%rdi
   0x000000000064c7b7 <+377>:	callq  0x652201 <u_freeheader>
   0x000000000064c7bc <+382>:	jmp    0x64c7f1 <u_savecommon+435>
   0x000000000064c7be <+384>:	mov    -0x28(%rbp),%rax
   0x000000000064c7c2 <+388>:	mov    0x10(%rax),%rax
   0x000000000064c7c6 <+392>:	mov    %rax,-0x28(%rbp)
   0x000000000064c7ca <+396>:	mov    -0x28(%rbp),%rax
   0x000000000064c7ce <+400>:	mov    0x10(%rax),%rax
   0x000000000064c7d2 <+404>:	test   %rax,%rax
   0x000000000064c7d5 <+407>:	jne    0x64c7be <u_savecommon+384>
   0x000000000064c7d7 <+409>:	mov    0x34b322(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c7de <+416>:	lea    -0x58(%rbp),%rdx
   0x000000000064c7e2 <+420>:	mov    -0x28(%rbp),%rcx
   0x000000000064c7e6 <+424>:	mov    %rcx,%rsi
   0x000000000064c7e9 <+427>:	mov    %rax,%rdi
   0x000000000064c7ec <+430>:	callq  0x6522f8 <u_freebranch>
   0x000000000064c7f1 <+435>:	mov    0x34b308(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c7f8 <+442>:	mov    0x1e00(%rax),%eax
   0x000000000064c7fe <+448>:	movslq %eax,%rbx
   0x000000000064c801 <+451>:	callq  0x64c57e <get_undolevel>
   0x000000000064c806 <+456>:	cmp    %rax,%rbx
   0x000000000064c809 <+459>:	jle    0x64c822 <u_savecommon+484>
   0x000000000064c80b <+461>:	mov    0x34b2ee(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c812 <+468>:	mov    0x1de8(%rax),%rax
   0x000000000064c819 <+475>:	test   %rax,%rax
   0x000000000064c81c <+478>:	jne    0x64c75d <u_savecommon+287>
   0x000000000064c822 <+484>:	cmpq   $0x0,-0x40(%rbp)
   0x000000000064c827 <+489>:	jne    0x64c865 <u_savecommon+551>
   0x000000000064c829 <+491>:	mov    -0x58(%rbp),%rax
   0x000000000064c82d <+495>:	test   %rax,%rax
   0x000000000064c830 <+498>:	je     0x64c84d <u_savecommon+527>
   0x000000000064c832 <+500>:	mov    -0x58(%rbp),%rcx
   0x000000000064c836 <+504>:	mov    0x34b2c3(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c83d <+511>:	mov    $0x0,%edx
   0x000000000064c842 <+516>:	mov    %rcx,%rsi
   0x000000000064c845 <+519>:	mov    %rax,%rdi
   0x000000000064c848 <+522>:	callq  0x6522f8 <u_freebranch>
   0x000000000064c84d <+527>:	mov    0x34b2ac(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c854 <+534>:	movb   $0x0,0x1e04(%rax)
   0x000000000064c85b <+541>:	mov    $0x1,%eax
   0x000000000064c860 <+546>:	jmpq   0x64ceec <u_savecommon+2222>
   0x000000000064c865 <+551>:	mov    -0x40(%rbp),%rax
   0x000000000064c869 <+555>:	movq   $0x0,0x8(%rax)
   0x000000000064c871 <+563>:	mov    0x34b288(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c878 <+570>:	mov    0x1df0(%rax),%rdx
   0x000000000064c87f <+577>:	mov    -0x40(%rbp),%rax
   0x000000000064c883 <+581>:	mov    %rdx,(%rax)
   0x000000000064c886 <+584>:	mov    -0x58(%rbp),%rdx
   0x000000000064c88a <+588>:	mov    -0x40(%rbp),%rax
   0x000000000064c88e <+592>:	mov    %rdx,0x10(%rax)
   0x000000000064c892 <+596>:	mov    -0x58(%rbp),%rax
   0x000000000064c896 <+600>:	test   %rax,%rax
   0x000000000064c899 <+603>:	je     0x64c8ff <u_savecommon+705>
   0x000000000064c89b <+605>:	mov    -0x58(%rbp),%rax
   0x000000000064c89f <+609>:	mov    0x18(%rax),%rdx
   0x000000000064c8a3 <+613>:	mov    -0x40(%rbp),%rax
   0x000000000064c8a7 <+617>:	mov    %rdx,0x18(%rax)
   0x000000000064c8ab <+621>:	mov    -0x40(%rbp),%rax
   0x000000000064c8af <+625>:	mov    0x18(%rax),%rax
   0x000000000064c8b3 <+629>:	test   %rax,%rax
   0x000000000064c8b6 <+632>:	je     0x64c8c8 <u_savecommon+650>
   0x000000000064c8b8 <+634>:	mov    -0x40(%rbp),%rax
   0x000000000064c8bc <+638>:	mov    0x18(%rax),%rax
   0x000000000064c8c0 <+642>:	mov    -0x40(%rbp),%rdx
   0x000000000064c8c4 <+646>:	mov    %rdx,0x10(%rax)
   0x000000000064c8c8 <+650>:	mov    -0x58(%rbp),%rax
   0x000000000064c8cc <+654>:	mov    -0x40(%rbp),%rdx
   0x000000000064c8d0 <+658>:	mov    %rdx,0x18(%rax)
   0x000000000064c8d4 <+662>:	mov    0x34b225(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c8db <+669>:	mov    0x1de8(%rax),%rdx
   0x000000000064c8e2 <+676>:	mov    -0x58(%rbp),%rax
   0x000000000064c8e6 <+680>:	cmp    %rax,%rdx
   0x000000000064c8e9 <+683>:	jne    0x64c90b <u_savecommon+717>
   0x000000000064c8eb <+685>:	mov    0x34b20e(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c8f2 <+692>:	mov    -0x40(%rbp),%rdx
   0x000000000064c8f6 <+696>:	mov    %rdx,0x1de8(%rax)
   0x000000000064c8fd <+703>:	jmp    0x64c90b <u_savecommon+717>
   0x000000000064c8ff <+705>:	mov    -0x40(%rbp),%rax
   0x000000000064c903 <+709>:	movq   $0x0,0x18(%rax)
   0x000000000064c90b <+717>:	mov    0x34b1ee(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c912 <+724>:	mov    0x1df0(%rax),%rax
   0x000000000064c919 <+731>:	test   %rax,%rax
   0x000000000064c91c <+734>:	je     0x64c934 <u_savecommon+758>
   0x000000000064c91e <+736>:	mov    0x34b1db(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c925 <+743>:	mov    0x1df0(%rax),%rax
   0x000000000064c92c <+750>:	mov    -0x40(%rbp),%rdx
   0x000000000064c930 <+754>:	mov    %rdx,0x8(%rax)
   0x000000000064c934 <+758>:	mov    0x34b1c5(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c93b <+765>:	mov    0x1e08(%rax),%rdx
   0x000000000064c942 <+772>:	add    $0x1,%rdx
   0x000000000064c946 <+776>:	mov    %rdx,0x1e08(%rax)
   0x000000000064c94d <+783>:	mov    0x1e08(%rax),%rdx
   0x000000000064c954 <+790>:	mov    -0x40(%rbp),%rax
   0x000000000064c958 <+794>:	mov    %rdx,0x20(%rax)
   0x000000000064c95c <+798>:	mov    0x34b19d(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c963 <+805>:	mov    -0x40(%rbp),%rdx
   0x000000000064c967 <+809>:	mov    0x20(%rdx),%rdx
   0x000000000064c96b <+813>:	mov    %rdx,0x1e18(%rax)
   0x000000000064c972 <+820>:	mov    $0x0,%edi
   0x000000000064c977 <+825>:	callq  0x42f4d0 <time@plt>
   0x000000000064c97c <+830>:	mov    %rax,%rdx
   0x000000000064c97f <+833>:	mov    -0x40(%rbp),%rax
   0x000000000064c983 <+837>:	mov    %rdx,0x498(%rax)
   0x000000000064c98a <+844>:	mov    -0x40(%rbp),%rax
   0x000000000064c98e <+848>:	movq   $0x0,0x4a0(%rax)
   0x000000000064c999 <+859>:	mov    0x34b160(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064c9a0 <+866>:	mov    -0x40(%rbp),%rdx
   0x000000000064c9a4 <+870>:	mov    0x498(%rdx),%rdx
   0x000000000064c9ab <+877>:	add    $0x1,%rdx
   0x000000000064c9af <+881>:	mov    %rdx,0x1e20(%rax)
   0x000000000064c9b6 <+888>:	mov    -0x40(%rbp),%rax
   0x000000000064c9ba <+892>:	movl   $0x0,0x28(%rax)
   0x000000000064c9c1 <+899>:	mov    -0x40(%rbp),%rax
   0x000000000064c9c5 <+903>:	movq   $0x0,0x30(%rax)
   0x000000000064c9cd <+911>:	mov    -0x40(%rbp),%rax
   0x000000000064c9d1 <+915>:	movq   $0x0,0x38(%rax)
   0x000000000064c9d9 <+923>:	mov    0x354b10(%rip),%rax        # 0x9a14f0 <curwin>
   0x000000000064c9e0 <+930>:	mov    -0x40(%rbp),%rcx
   0x000000000064c9e4 <+934>:	mov    0x40(%rax),%rdx
   0x000000000064c9e8 <+938>:	mov    0x38(%rax),%rax
   0x000000000064c9ec <+942>:	mov    %rax,0x40(%rcx)
   0x000000000064c9f0 <+946>:	mov    %rdx,0x48(%rcx)
   0x000000000064c9f4 <+950>:	callq  0x628c42 <virtual_active>
   0x000000000064c9f9 <+955>:	test   %eax,%eax
   0x000000000064c9fb <+957>:	je     0x64ca1d <u_savecommon+991>
   0x000000000064c9fd <+959>:	mov    0x354aec(%rip),%rax        # 0x9a14f0 <curwin>
   0x000000000064ca04 <+966>:	mov    0x44(%rax),%eax
   0x000000000064ca07 <+969>:	test   %eax,%eax
   0x000000000064ca09 <+971>:	jle    0x64ca1d <u_savecommon+991>
   0x000000000064ca0b <+973>:	callq  0x45af20 <getviscol>
   0x000000000064ca10 <+978>:	movslq %eax,%rdx
   0x000000000064ca13 <+981>:	mov    -0x40(%rbp),%rax
   0x000000000064ca17 <+985>:	mov    %rdx,0x50(%rax)
   0x000000000064ca1b <+989>:	jmp    0x64ca29 <u_savecommon+1003>
   0x000000000064ca1d <+991>:	mov    -0x40(%rbp),%rax
   0x000000000064ca21 <+995>:	movq   $0xffffffffffffffff,0x50(%rax)
   0x000000000064ca29 <+1003>:	mov    0x34b0d0(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064ca30 <+1010>:	mov    0xc0(%rax),%eax
   0x000000000064ca36 <+1016>:	test   %eax,%eax
   0x000000000064ca38 <+1018>:	setne  %al
   0x000000000064ca3b <+1021>:	movzbl %al,%edx
   0x000000000064ca3e <+1024>:	mov    0x34b0bb(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064ca45 <+1031>:	mov    0x18(%rax),%eax
   0x000000000064ca48 <+1034>:	and    $0x1,%eax
   0x000000000064ca4b <+1037>:	test   %eax,%eax
   0x000000000064ca4d <+1039>:	je     0x64ca56 <u_savecommon+1048>
   0x000000000064ca4f <+1041>:	mov    $0x2,%eax
   0x000000000064ca54 <+1046>:	jmp    0x64ca5b <u_savecommon+1053>
   0x000000000064ca56 <+1048>:	mov    $0x0,%eax
   0x000000000064ca5b <+1053>:	add    %eax,%edx
   0x000000000064ca5d <+1055>:	mov    -0x40(%rbp),%rax
   0x000000000064ca61 <+1059>:	mov    %edx,0x58(%rax)
   0x000000000064ca64 <+1062>:	mov    0x34b095(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064ca6b <+1069>:	add    $0x110,%rax
   0x000000000064ca71 <+1075>:	mov    %rax,%rdi
   0x000000000064ca74 <+1078>:	callq  0x64c5cd <zero_fmark_additional_data>
   0x000000000064ca79 <+1083>:	mov    0x34b080(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064ca80 <+1090>:	lea    0x110(%rax),%rcx
   0x000000000064ca87 <+1097>:	mov    -0x40(%rbp),%rax
   0x000000000064ca8b <+1101>:	add    $0x60,%rax
   0x000000000064ca8f <+1105>:	mov    $0x410,%edx
   0x000000000064ca94 <+1110>:	mov    %rcx,%rsi
   0x000000000064ca97 <+1113>:	mov    %rax,%rdi
   0x000000000064ca9a <+1116>:	callq  0x42ee90 <memmove@plt>
   0x000000000064ca9f <+1121>:	mov    0x34b05a(%rip),%rdx        # 0x997b00 <curbuf>
   0x000000000064caa6 <+1128>:	mov    -0x40(%rbp),%rax
   0x000000000064caaa <+1132>:	mov    0x520(%rdx),%rcx
   0x000000000064cab1 <+1139>:	mov    %rcx,0x470(%rax)
   0x000000000064cab8 <+1146>:	mov    0x528(%rdx),%rcx
   0x000000000064cabf <+1153>:	mov    %rcx,0x478(%rax)
   0x000000000064cac6 <+1160>:	mov    0x530(%rdx),%rcx
   0x000000000064cacd <+1167>:	mov    %rcx,0x480(%rax)
   0x000000000064cad4 <+1174>:	mov    0x538(%rdx),%rcx
   0x000000000064cadb <+1181>:	mov    %rcx,0x488(%rax)
   0x000000000064cae2 <+1188>:	mov    0x540(%rdx),%rdx
   0x000000000064cae9 <+1195>:	mov    %rdx,0x490(%rax)
   0x000000000064caf0 <+1202>:	mov    0x34b009(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064caf7 <+1209>:	mov    -0x40(%rbp),%rdx
   0x000000000064cafb <+1213>:	mov    %rdx,0x1df0(%rax)
   0x000000000064cb02 <+1220>:	mov    0x34aff7(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cb09 <+1227>:	mov    0x1de8(%rax),%rax
   0x000000000064cb10 <+1234>:	test   %rax,%rax
   0x000000000064cb13 <+1237>:	jne    0x64cb27 <u_savecommon+1257>
   0x000000000064cb15 <+1239>:	mov    0x34afe4(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cb1c <+1246>:	mov    -0x40(%rbp),%rdx
   0x000000000064cb20 <+1250>:	mov    %rdx,0x1de8(%rax)
   0x000000000064cb27 <+1257>:	mov    0x34afd2(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cb2e <+1264>:	mov    0x1e00(%rax),%edx
   0x000000000064cb34 <+1270>:	add    $0x1,%edx
   0x000000000064cb37 <+1273>:	mov    %edx,0x1e00(%rax)
   0x000000000064cb3d <+1279>:	jmpq   0x64cd57 <u_savecommon+1817>
   0x000000000064cb42 <+1284>:	callq  0x64c57e <get_undolevel>
   0x000000000064cb47 <+1289>:	test   %rax,%rax
   0x000000000064cb4a <+1292>:	jns    0x64cb56 <u_savecommon+1304>
   0x000000000064cb4c <+1294>:	mov    $0x1,%eax
   0x000000000064cb51 <+1299>:	jmpq   0x64ceec <u_savecommon+2222>
   0x000000000064cb56 <+1304>:	cmpq   $0x1,-0x20(%rbp)
   0x000000000064cb5b <+1309>:	jne    0x64cd52 <u_savecommon+1812>
   0x000000000064cb61 <+1315>:	callq  0x6520b2 <u_get_headentry>
   0x000000000064cb66 <+1320>:	mov    %rax,-0x38(%rbp)
   0x000000000064cb6a <+1324>:	movq   $0x0,-0x30(%rbp)
   0x000000000064cb72 <+1332>:	movq   $0x0,-0x48(%rbp)
   0x000000000064cb7a <+1340>:	jmpq   0x64cd44 <u_savecommon+1798>
   0x000000000064cb7f <+1345>:	cmpq   $0x0,-0x38(%rbp)
   0x000000000064cb84 <+1350>:	je     0x64cd51 <u_savecommon+1811>
   0x000000000064cb8a <+1356>:	mov    0x34af6f(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cb91 <+1363>:	mov    0x1df0(%rax),%rax
   0x000000000064cb98 <+1370>:	mov    0x38(%rax),%rax
   0x000000000064cb9c <+1374>:	cmp    -0x38(%rbp),%rax
   0x000000000064cba0 <+1378>:	je     0x64cbea <u_savecommon+1452>
   0x000000000064cba2 <+1380>:	mov    -0x38(%rbp),%rax
   0x000000000064cba6 <+1384>:	mov    0x8(%rax),%rdx
   0x000000000064cbaa <+1388>:	mov    -0x38(%rbp),%rax
   0x000000000064cbae <+1392>:	mov    0x28(%rax),%rax
   0x000000000064cbb2 <+1396>:	add    %rdx,%rax
   0x000000000064cbb5 <+1399>:	lea    0x1(%rax),%rdx
   0x000000000064cbb9 <+1403>:	mov    -0x38(%rbp),%rax
   0x000000000064cbbd <+1407>:	mov    0x10(%rax),%rax
   0x000000000064cbc1 <+1411>:	test   %rax,%rax
   0x000000000064cbc4 <+1414>:	jne    0x64cbd7 <u_savecommon+1433>
   0x000000000064cbc6 <+1416>:	mov    0x34af33(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cbcd <+1423>:	mov    0x8(%rax),%rax
   0x000000000064cbd1 <+1427>:	add    $0x1,%rax
   0x000000000064cbd5 <+1431>:	jmp    0x64cbdf <u_savecommon+1441>
   0x000000000064cbd7 <+1433>:	mov    -0x38(%rbp),%rax
   0x000000000064cbdb <+1437>:	mov    0x10(%rax),%rax
   0x000000000064cbdf <+1441>:	cmp    %rax,%rdx
   0x000000000064cbe2 <+1444>:	jne    0x64cd52 <u_savecommon+1812>
   0x000000000064cbe8 <+1450>:	jmp    0x64cc06 <u_savecommon+1480>
   0x000000000064cbea <+1452>:	mov    -0x38(%rbp),%rax
   0x000000000064cbee <+1456>:	mov    0x18(%rax),%rdx
   0x000000000064cbf2 <+1460>:	mov    0x34af07(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cbf9 <+1467>:	mov    0x8(%rax),%rax
   0x000000000064cbfd <+1471>:	cmp    %rax,%rdx
   0x000000000064cc00 <+1474>:	jne    0x64cd52 <u_savecommon+1812>
   0x000000000064cc06 <+1480>:	mov    -0x38(%rbp),%rax
   0x000000000064cc0a <+1484>:	mov    0x28(%rax),%rax
   0x000000000064cc0e <+1488>:	cmp    $0x1,%rax
   0x000000000064cc12 <+1492>:	jle    0x64cc4a <u_savecommon+1548>
   0x000000000064cc14 <+1494>:	mov    -0x38(%rbp),%rax
   0x000000000064cc18 <+1498>:	mov    0x8(%rax),%rax
   0x000000000064cc1c <+1502>:	cmp    -0x68(%rbp),%rax
   0x000000000064cc20 <+1506>:	jg     0x64cc4a <u_savecommon+1548>
   0x000000000064cc22 <+1508>:	mov    -0x68(%rbp),%rax
   0x000000000064cc26 <+1512>:	lea    0x2(%rax),%rcx
   0x000000000064cc2a <+1516>:	mov    -0x38(%rbp),%rax
   0x000000000064cc2e <+1520>:	mov    0x8(%rax),%rdx
   0x000000000064cc32 <+1524>:	mov    -0x38(%rbp),%rax
   0x000000000064cc36 <+1528>:	mov    0x28(%rax),%rax
   0x000000000064cc3a <+1532>:	add    %rdx,%rax
   0x000000000064cc3d <+1535>:	add    $0x1,%rax
   0x000000000064cc41 <+1539>:	cmp    %rax,%rcx
   0x000000000064cc44 <+1542>:	jle    0x64cd52 <u_savecommon+1812>
   0x000000000064cc4a <+1548>:	mov    -0x38(%rbp),%rax
   0x000000000064cc4e <+1552>:	mov    0x28(%rax),%rax
   0x000000000064cc52 <+1556>:	cmp    $0x1,%rax
   0x000000000064cc56 <+1560>:	jne    0x64cd2c <u_savecommon+1774>
   0x000000000064cc5c <+1566>:	mov    -0x38(%rbp),%rax
   0x000000000064cc60 <+1570>:	mov    0x8(%rax),%rax
   0x000000000064cc64 <+1574>:	cmp    -0x68(%rbp),%rax
   0x000000000064cc68 <+1578>:	jne    0x64cd2c <u_savecommon+1774>
   0x000000000064cc6e <+1584>:	cmpq   $0x0,-0x48(%rbp)
   0x000000000064cc73 <+1589>:	jle    0x64ccc5 <u_savecommon+1671>
   0x000000000064cc75 <+1591>:	callq  0x65210d <u_getbot>
   0x000000000064cc7a <+1596>:	mov    0x34ae7f(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cc81 <+1603>:	movb   $0x0,0x1e04(%rax)
   0x000000000064cc88 <+1610>:	mov    -0x38(%rbp),%rax
   0x000000000064cc8c <+1614>:	mov    (%rax),%rdx
   0x000000000064cc8f <+1617>:	mov    -0x30(%rbp),%rax
   0x000000000064cc93 <+1621>:	mov    %rdx,(%rax)
   0x000000000064cc96 <+1624>:	mov    0x34ae63(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cc9d <+1631>:	mov    0x1df0(%rax),%rax
   0x000000000064cca4 <+1638>:	mov    0x30(%rax),%rdx
   0x000000000064cca8 <+1642>:	mov    -0x38(%rbp),%rax
   0x000000000064ccac <+1646>:	mov    %rdx,(%rax)
   0x000000000064ccaf <+1649>:	mov    0x34ae4a(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064ccb6 <+1656>:	mov    0x1df0(%rax),%rax
   0x000000000064ccbd <+1663>:	mov    -0x38(%rbp),%rdx
   0x000000000064ccc1 <+1667>:	mov    %rdx,0x30(%rax)
   0x000000000064ccc5 <+1671>:	cmpq   $0x0,-0x78(%rbp)
   0x000000000064ccca <+1676>:	je     0x64ccda <u_savecommon+1692>
   0x000000000064cccc <+1678>:	mov    -0x38(%rbp),%rax
   0x000000000064ccd0 <+1682>:	mov    -0x78(%rbp),%rdx
   0x000000000064ccd4 <+1686>:	mov    %rdx,0x10(%rax)
   0x000000000064ccd8 <+1690>:	jmp    0x64cd22 <u_savecommon+1764>
   0x000000000064ccda <+1692>:	mov    0x34ae1f(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cce1 <+1699>:	mov    0x8(%rax),%rax
   0x000000000064cce5 <+1703>:	cmp    -0x70(%rbp),%rax
   0x000000000064cce9 <+1707>:	jge    0x64ccf9 <u_savecommon+1723>
   0x000000000064cceb <+1709>:	mov    -0x38(%rbp),%rax
   0x000000000064ccef <+1713>:	movq   $0x0,0x10(%rax)
   0x000000000064ccf7 <+1721>:	jmp    0x64cd22 <u_savecommon+1764>
   0x000000000064ccf9 <+1723>:	mov    0x34ae00(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cd00 <+1730>:	mov    0x8(%rax),%rdx
   0x000000000064cd04 <+1734>:	mov    -0x38(%rbp),%rax
   0x000000000064cd08 <+1738>:	mov    %rdx,0x18(%rax)
   0x000000000064cd0c <+1742>:	mov    0x34aded(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cd13 <+1749>:	mov    0x1df0(%rax),%rax
   0x000000000064cd1a <+1756>:	mov    -0x38(%rbp),%rdx
   0x000000000064cd1e <+1760>:	mov    %rdx,0x38(%rax)
   0x000000000064cd22 <+1764>:	mov    $0x1,%eax
   0x000000000064cd27 <+1769>:	jmpq   0x64ceec <u_savecommon+2222>
   0x000000000064cd2c <+1774>:	mov    -0x38(%rbp),%rax
   0x000000000064cd30 <+1778>:	mov    %rax,-0x30(%rbp)
   0x000000000064cd34 <+1782>:	mov    -0x38(%rbp),%rax
   0x000000000064cd38 <+1786>:	mov    (%rax),%rax
   0x000000000064cd3b <+1789>:	mov    %rax,-0x38(%rbp)
   0x000000000064cd3f <+1793>:	addq   $0x1,-0x48(%rbp)
   0x000000000064cd44 <+1798>:	cmpq   $0x9,-0x48(%rbp)
   0x000000000064cd49 <+1803>:	jle    0x64cb7f <u_savecommon+1345>
   0x000000000064cd4f <+1809>:	jmp    0x64cd52 <u_savecommon+1812>
   0x000000000064cd51 <+1811>:	nop
   0x000000000064cd52 <+1812>:	callq  0x65210d <u_getbot>
   0x000000000064cd57 <+1817>:	mov    $0x30,%edi
   0x000000000064cd5c <+1822>:	callq  0x54bdd4 <xmalloc>
   0x000000000064cd61 <+1827>:	mov    %rax,-0x38(%rbp)
   0x000000000064cd65 <+1831>:	mov    -0x38(%rbp),%rax
   0x000000000064cd69 <+1835>:	mov    $0x30,%edx
   0x000000000064cd6e <+1840>:	mov    $0x0,%esi
   0x000000000064cd73 <+1845>:	mov    %rax,%rdi
   0x000000000064cd76 <+1848>:	callq  0x42e600 <memset@plt>
   0x000000000064cd7b <+1853>:	mov    -0x38(%rbp),%rax
   0x000000000064cd7f <+1857>:	mov    -0x20(%rbp),%rdx
   0x000000000064cd83 <+1861>:	mov    %rdx,0x28(%rax)
   0x000000000064cd87 <+1865>:	mov    -0x38(%rbp),%rax
   0x000000000064cd8b <+1869>:	mov    -0x68(%rbp),%rdx
   0x000000000064cd8f <+1873>:	mov    %rdx,0x8(%rax)
   0x000000000064cd93 <+1877>:	cmpq   $0x0,-0x78(%rbp)
   0x000000000064cd98 <+1882>:	je     0x64cda8 <u_savecommon+1898>
   0x000000000064cd9a <+1884>:	mov    -0x38(%rbp),%rax
   0x000000000064cd9e <+1888>:	mov    -0x78(%rbp),%rdx
   0x000000000064cda2 <+1892>:	mov    %rdx,0x10(%rax)
   0x000000000064cda6 <+1896>:	jmp    0x64cdf0 <u_savecommon+1970>
   0x000000000064cda8 <+1898>:	mov    0x34ad51(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cdaf <+1905>:	mov    0x8(%rax),%rax
   0x000000000064cdb3 <+1909>:	cmp    -0x70(%rbp),%rax
   0x000000000064cdb7 <+1913>:	jge    0x64cdc7 <u_savecommon+1929>
   0x000000000064cdb9 <+1915>:	mov    -0x38(%rbp),%rax
   0x000000000064cdbd <+1919>:	movq   $0x0,0x10(%rax)
   0x000000000064cdc5 <+1927>:	jmp    0x64cdf0 <u_savecommon+1970>
   0x000000000064cdc7 <+1929>:	mov    0x34ad32(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cdce <+1936>:	mov    0x8(%rax),%rdx
   0x000000000064cdd2 <+1940>:	mov    -0x38(%rbp),%rax
   0x000000000064cdd6 <+1944>:	mov    %rdx,0x18(%rax)
   0x000000000064cdda <+1948>:	mov    0x34ad1f(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cde1 <+1955>:	mov    0x1df0(%rax),%rax
   0x000000000064cde8 <+1962>:	mov    -0x38(%rbp),%rdx
   0x000000000064cdec <+1966>:	mov    %rdx,0x38(%rax)
   0x000000000064cdf0 <+1970>:	cmpq   $0x0,-0x20(%rbp)
   0x000000000064cdf5 <+1975>:	jle    0x64ce94 <u_savecommon+2134>
   0x000000000064cdfb <+1981>:	mov    -0x20(%rbp),%rax
   0x000000000064cdff <+1985>:	shl    $0x3,%rax
   0x000000000064ce03 <+1989>:	mov    %rax,%rdi
   0x000000000064ce06 <+1992>:	callq  0x54bdd4 <xmalloc>
   0x000000000064ce0b <+1997>:	mov    %rax,%rdx
   0x000000000064ce0e <+2000>:	mov    -0x38(%rbp),%rax
   0x000000000064ce12 <+2004>:	mov    %rdx,0x20(%rax)
   0x000000000064ce16 <+2008>:	movq   $0x0,-0x48(%rbp)
   0x000000000064ce1e <+2016>:	mov    -0x68(%rbp),%rax
   0x000000000064ce22 <+2020>:	add    $0x1,%rax
   0x000000000064ce26 <+2024>:	mov    %rax,-0x50(%rbp)
   0x000000000064ce2a <+2028>:	jmp    0x64ce88 <u_savecommon+2122>
   0x000000000064ce2c <+2030>:	callq  0x55d4a7 <fast_breakcheck>
   0x000000000064ce31 <+2035>:	mov    0x34ae51(%rip),%eax        # 0x997c88 <got_int>
   0x000000000064ce37 <+2041>:	test   %eax,%eax
   0x000000000064ce39 <+2043>:	je     0x64ce58 <u_savecommon+2074>
   0x000000000064ce3b <+2045>:	mov    -0x48(%rbp),%rdx
   0x000000000064ce3f <+2049>:	mov    -0x38(%rbp),%rax
   0x000000000064ce43 <+2053>:	mov    %rdx,%rsi
   0x000000000064ce46 <+2056>:	mov    %rax,%rdi
   0x000000000064ce49 <+2059>:	callq  0x6524ad <u_freeentry>
   0x000000000064ce4e <+2064>:	mov    $0x0,%eax
   0x000000000064ce53 <+2069>:	jmpq   0x64ceec <u_savecommon+2222>
   0x000000000064ce58 <+2074>:	mov    -0x38(%rbp),%rax
   0x000000000064ce5c <+2078>:	mov    0x20(%rax),%rax
   0x000000000064ce60 <+2082>:	mov    -0x48(%rbp),%rdx
   0x000000000064ce64 <+2086>:	shl    $0x3,%rdx
   0x000000000064ce68 <+2090>:	lea    (%rax,%rdx,1),%rbx
   0x000000000064ce6c <+2094>:	mov    -0x50(%rbp),%rax
   0x000000000064ce70 <+2098>:	lea    0x1(%rax),%rdx
   0x000000000064ce74 <+2102>:	mov    %rdx,-0x50(%rbp)
   0x000000000064ce78 <+2106>:	mov    %rax,%rdi
   0x000000000064ce7b <+2109>:	callq  0x6528af <u_save_line>
   0x000000000064ce80 <+2114>:	mov    %rax,(%rbx)
   0x000000000064ce83 <+2117>:	addq   $0x1,-0x48(%rbp)
   0x000000000064ce88 <+2122>:	mov    -0x48(%rbp),%rax
   0x000000000064ce8c <+2126>:	cmp    -0x20(%rbp),%rax
   0x000000000064ce90 <+2130>:	jl     0x64ce2c <u_savecommon+2030>
   0x000000000064ce92 <+2132>:	jmp    0x64cea0 <u_savecommon+2146>
   0x000000000064ce94 <+2134>:	mov    -0x38(%rbp),%rax
   0x000000000064ce98 <+2138>:	movq   $0x0,0x20(%rax)
   0x000000000064cea0 <+2146>:	mov    0x34ac59(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cea7 <+2153>:	mov    0x1df0(%rax),%rax
   0x000000000064ceae <+2160>:	mov    0x30(%rax),%rdx
   0x000000000064ceb2 <+2164>:	mov    -0x38(%rbp),%rax
   0x000000000064ceb6 <+2168>:	mov    %rdx,(%rax)
   0x000000000064ceb9 <+2171>:	mov    0x34ac40(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064cec0 <+2178>:	mov    0x1df0(%rax),%rax
   0x000000000064cec7 <+2185>:	mov    -0x38(%rbp),%rdx
   0x000000000064cecb <+2189>:	mov    %rdx,0x30(%rax)
   0x000000000064cecf <+2193>:	mov    0x34ac2a(%rip),%rax        # 0x997b00 <curbuf>
   0x000000000064ced6 <+2200>:	movb   $0x0,0x1e04(%rax)
   0x000000000064cedd <+2207>:	movl   $0x0,0x350121(%rip)        # 0x99d008 <undo_undoes>
   0x000000000064cee7 <+2217>:	mov    $0x1,%eax
   0x000000000064ceec <+2222>:	mov    -0x18(%rbp),%rbx
   0x000000000064cef0 <+2226>:	xor    %fs:0x28,%rbx
   0x000000000064cef9 <+2235>:	je     0x64cf00 <u_savecommon+2242>
   0x000000000064cefb <+2237>:	callq  0x42eff0 <__stack_chk_fail@plt>
   0x000000000064cf00 <+2242>:	add    $0x78,%rsp
   0x000000000064cf04 <+2246>:	pop    %rbx
   0x000000000064cf05 <+2247>:	pop    %rbp
   0x000000000064cf06 <+2248>:	retq   
End of assembler dump.
(gdb) 
vshcmd: > # I quite often accidentaly end up with way too much text in gdb
vshcmd: > # when looking around a command, and I end up losing the
vshcmd: > # information I found above.
vshcmd: > # Vsh is useful in that manner because the normal way of using it
vshcmd: > # means that you replace the output of a command until you find what
vshcmd: > # you want.
vshcmd: > # apropos variable
vshcmd: > # help info variable
vshcmd: > # u
vshcmd: > # help set variable
vshcmd: > # u
vshcmd: > apropos variable
Evaluate expression EXP and assign result to variable VAR, using assignment
syntax appropriate for the current language (VAR = EXP or VAR := EXP for
example).  VAR may be a debugger "convenience" variable (names starting
with $), a register (a few standard names starting with $), or an actual
variable in the program being debugged.  EXP is any valid expression.
This may usually be abbreviated to simply "set".
(gdb) quit
neovim [14:18:05] $ ^C
neovim [14:18:06] $ ^C
neovim [14:18:07] $ 
vshcmd: > # The control keys (C-c C-d C-z etc) can be sent with the keybinding
vshcmd: > # '<localleader>c', press what type of control key you want to send
vshcmd: > # after that.
vshcmd: > fg
sleep 10
neovim [14:15:37] $ 
vshcmd: > cat demo_part3.vsh

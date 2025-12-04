global  main
extern  puts
extern  fflush
extern  putchar
extern 	atoi
extern  factorial_message

section .data

hello_str	db      "Hello, World!", 0
menu_str	db	"usage: <command> <arg1> <arg2>", 0xD, 0xA, 0xD, 0xA,  \
			"commands:", 0xD, 0xA, \
			"  e   Echo arguments arg1 and arg2", 0xD, 0xA, \
			"  m   Prints out a magic number", 0xD, 0xA, \
			"  +   Adds together arg1 and arg2", 0xD, 0xA, \
			"  !   Prints out the factorial value of", 0xD, 0xA, \
			"      arg1, as well as the message in arg2", 0xD, 0xA, 0
argc		db 0,0
argv		dq 0,0,0

section .text


main:
	mov rbp, rdi	; argc in rdi
	mov rbx, rsi	; argv in rsi

	mov rax, 4	; if not 2 args (+1 cmp) then error menu
	cmp rbp, rax
	jne error_menu

	;mov rdi, argc		; mov destination reg to point to argc
	;mov cl, bpl
	;add cl, 48		; Makes number into ASCII (48 = '0')
	;mov byte [rdi], cl	; write into argc data buffer
	;call puts
	

	mov rdi, argv	; dest argv

.loop_argv:

	mov r12, [rbx]	; argv[0] in r12
	mov [rdi], r12	; argv_data[0] = r12 

	add rbx, 8	; +8 byte means [0] -> [1]
	add rdi, 8
	dec rbp		; argc--;
	jnz .loop_argv	; ZF is not true.

.parse_cmd:
	mov rsi, argv
	mov rbx, [rsi+8]	; rbx = argv[1] <cmd>
	mov al, byte [rbx]
	cmp al, 'e'		; rbx is pointer to string
	je echo

	cmp al, 'm'
	je magic

	cmp al, '+'
	je addition

	cmp al, '!'
	je factorial

	jmp error_menu


; ====================================================
; echo():
; 	calls puts(argv[1], \n) puts(argv[2], \n)
; ====================================================
echo:
	mov 	rdi, [argv + 16]	; dest = &(char* argv[2])
	call 	puts
	mov 	rdi, [argv + 24]	; dset = &(char* argv[3])
	call 	puts
	mov	rax, 0		; exit 0
	ret


; ====================================================
; magic():
; 	calls print_int(-126)
; ====================================================
magic:
        mov 	rdi, -126
	call 	print_int
	mov 	rax, 0
	ret


; ====================================================
; addition():
; 	converts args to int, adds numbers, prints result.
; ====================================================
addition:
	push 	r12
	push	r13

	mov 	rdi, [argv + 16]
	call	atoi
	mov 	r12, rax

	mov 	rdi, [argv + 24]
	call	atoi
	mov 	r13, rax
	
	add 	r12, r13
	mov	rdi, r12
	call 	print_int

	pop	r13
	pop	r12
	mov 	rax, 0
	ret
	

; ====================================================
; factorial():
; 	converts arg1 to int, arg2 is string (char*)
;	calls factorial_message.
; ====================================================
factorial:

	mov 	rdi, [argv + 16]
	call	atoi
	mov 	rdi, rax

	mov 	rax, [argv + 24]	; str (2nd arg)
	mov 	rsi, rax
	call 	factorial_message

	mov 	rdi, rax
	call	print_int

	mov 	rax, 0
	ret


error_menu:
        mov     rdi, menu_str
        call    puts
        mov     rdi, 0
        call    fflush
	mov 	rax, 1	; exit 1
        ret




; ====================================================
; print_int(uint64_t x):
; 	param x = rdi
;	if (x >= i || (x == 0 && i == 1) || b > 0) then true
;	   ( x < i && (x != 0 || i != 1) && b <= 0) then false
; ====================================================
print_int:

	push rbx
	push r12
	push r13

	mov rbx, rdi
	mov r13, 1000000
	mov r12, 0

	cmp rbx, 0
	jge while_loop

negative:
	mov rdi, 45	; "-"
	call putchar
	neg rbx

while_loop:
	cmp r13, 0
	je .exit_loop

.checkA:
	cmp rbx, r13	; x >= i
	jge .write_char	

.checkB:
	cmp rbx, 0
	jne .checkC	; (x == 0 && i == 1) 
	cmp r13, 1
	je .write_char	

.checkC:
	cmp r12, 0	
	jle .next_digit	; (b > 0) --> !(b <= 0)
	
.write_char:	
	mov rdx, 0
	mov rax, rbx	; rax = x
	div r13		; rdx:rax/r13 -> x/i  result in rax.

	add rax, 48		; Makes number into ASCII (48 = '0')
	mov rdi, rax
	call putchar
	mov r12, 1

.next_digit:
	mov rdx, 0
	mov rax, rbx	; rax = x
	idiv r13	; rdx:rax / r13 -> x / i
	mov rbx, rdx	; rdx = remainder (x%i)
	
	mov rdx, 0
	mov rax, r13	
	mov rcx, 10
	div rcx		; i = i / 10
	mov r13, rax

	jmp while_loop

.exit_loop:
	mov rdi, 0xA	; '\n'
	call putchar

	pop r13
	pop r12
	pop rbx
	
	ret







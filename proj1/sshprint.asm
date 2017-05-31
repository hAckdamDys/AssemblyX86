PUTSTR macro Sourc
	push dx
	push ax
	lea dx,Sourc
	mov ah,9h
	int 21h
	pop ax
	pop dx
endm
PUTCHA macro Sourc
	push dx
	push ax
	mov dl,Sourc
	mov ah,2h
	int 21h
	pop ax
	pop dx
endm
IFWHITEJUMPTO macro Dest
	cmp al, 27			;escape
	je Endonifikacja	;tutaj zapisujemy jak sie nazywa koniec
	cmp al, 20h
	je Dest
	cmp al, 09h
	je Dest
endm
;____________________________________________________________________________________________________________________

dane1 segment

Args		db 100h dup(?)			;256 bajtow na 256 znakow
P_Args		dw 40h dup(?)			;64 adresow na argi wazne to slowa DW!!!! gdy chcemy kolejny to albo incx2 albo add 2
Newline		db 0ah,0dh,"$"			;newline + karetka na lewo, db czyli ten wrzucamy do np al
Key			db 16 dup(?),"$"		;16 bajtow na poczatku
Mod1		db ?					;1 bajt na klucz
Ramka		db 17 dup(9 dup(0))		;153=17*9 bajtow na ramke, w niej zapiszemy ilosc odwiedzin kazdego punktu
Rows		db 9					;Ilosc rzedow w ramce 
Cols		db 17					;Kolumny w ramce
AsciiTab	db " .o+=*BOX@%&#/^"	;taby jakie wpisujemy

No_arg_err	db "Brak argumentow! Wypisz argumenty:",0ah,0dh,"$";w przypadku braku argumentow
Too_many_arg_err db "Za duzo argumentow! Wypisz ponownie:",0ah,0dh,"$";gdy za duzo argumentow
One_arg_err db "Wpisz drugi argument:",0ah,0dh,"$";gdy tylko jeden
One_arg_too_many_err db "Wpisz tylko drugi argument, nic wiecej:",0ah,0dh,"$";gdy wiecej niz 1 jak potrzebujemy tylko drgui
Bad_mod_err  db "Pierwszy argument powinien miec 1 znak; '0' lub '1':",0ah,0dh,"$";gdy za dlugi lub zla wartosc
Bad_key_num_err  db "Klucz powinien skladac sie z 32 znakow:",0ah,0dh,"$"
Bad_key_char_err db "Klucz powinien skladac sie ze znakow (0-9) lub MALYCH liter (a-f):",0ah,0dh,"$"

dane1 ends
;____________________________________________________________________________________________________________________


code segment

start1:
;____________________________________________________________________________________________________________________
;stos_init:
	mov ax, stos1			;rownowaznie: "mov ax, seg wstosu"
	mov	ss, ax
	mov	sp, offset wstosu 	;wstosu jest na koncu stosu czyli tam gdzie sp powinien wskazywac
;____________________________________________________________________________________________________________________
;zanim dam segment danych do ds to mam tam w ds:080h ilosc znakow oraz:
;znaki po programie ktore zaczynaja sie od ds:081h, domyslnie tam jest spacja i jakies wejscie
;uzyjemy dodatkowego segmentu zeby wrzucic do danych wejscie  
;____________________________________________________________________________________________________________________
call Parser	
;____________________________________________________________________________________________________________________
;teraz w P_Args mamy Ptry do Argsow
;a w bp ilosc argumentow
;           w Args tak: $Arg1%Arg2$Arg3$Arg4$
;a P_Args wskazuje tak: 1^   2^   3^   4^   5^

;init danych
	mov ax, seg Args		;rownie dobrze dzialaloby mov ax, dane1
	mov ds, ax
;sprawdzanie argumentow
;____________________________________________________________________________________________________________________
Check_too_many:
	PUTSTR Newline
	cmp bp,2
	jng Two_or_one_arg	;jesli nie wieksze niz 2
	PUTSTR Too_many_arg_err
	call Get_args_again
	jmp Check_too_many
Two_or_one_arg:
	;najpierw sprawdzmy czy pierwszy jest dobry
	mov bx, offset P_Args
	call GetArgSize			;potrzebuje w bx offset do tego arga co sprawdzamy
	cmp al,1
	je First_arg_good_size
	PUTSTR Bad_mod_err
	call Get_args_again
	jmp Check_too_many
First_arg_good_size:
	mov bx, word ptr ds:[bx];bx to teraz wskaznik na arga bezposrednio
	mov al, byte ptr ds:[bx];w al teraz znak jaki wpisalismy, wiemy ze tylko jeden jest
	cmp al, 49
	je Good_mod
	cmp al, 48
	je Good_mod
	PUTSTR Bad_mod_err
	call Get_args_again
	jmp Check_too_many
Good_mod:
	lea bx, Mod1
	sub al, 48
	mov byte ptr ds:[bx],al
	cmp bp,2				;czy wgl mamy drugi argument
	je Check_key_as_second	;sprawdzamy drugi arg jako drugi
	;jesli nie:
	PUTSTR One_arg_err
Get_key:
	call Get_args_again
	PUTSTR Newline
	cmp bp,1				;teraz potrzebujemy tylko jednego
	je Check_key_as_first	;sprawdzamy jako pierwszy
	PUTSTR One_arg_too_many_err
	jmp Get_key
Check_key_as_second:
	mov bx, offset P_Args
	add bx, 2
	jmp Check_key_bx_ready
Check_key_as_first:
	mov bx, offset P_Args
Check_key_bx_ready:
	call GetArgSize
	cmp al, 32
	je Check_key_chars
	PUTSTR Bad_key_num_err
	jmp Get_key
Bad_key_char:
	pop bx
	PUTSTR Bad_key_char_err
	jmp Get_key
Check_key_chars:			;gdy mamy dobra ilosc
	mov bx, word ptr ds:[bx];teraz w bx wskaznik na pierwszy char
	push bx					;zapiszmy ten wskaznik
	mov cx, 33 				;w cx mamy ilosc charow
	dec bx
Check_another_key_char:
	dec cx
	cmp cx,0
	je Good_key
	inc bx
	mov al, byte ptr ds:[bx];w al char na ktorego wskazuje bx
	cmp al, 48				;porownujemy z '0'
	jl Bad_key_char				;jak mniejsze to zly
	cmp al, 57				;porownujemy z '9'
	jle Check_another_key_char;jak mniejsze lub rowne to sprawdzamy kolejne
	cmp al, 97				;porownujemy z 'a'
	jl Bad_key_char				;jak mniejsze to pomiedzy 'a' a '9' wiec zle
	cmp al, 102				;z 'f'
	jle Check_another_key_char
	jmp Bad_key_char				;jak wieksze od 'f' to zle
;____________________________________________________________________________________________________________________
;dane sprawdzone poprawne
;przepisanie klucza do 16 bajtowej zmiennej key
;____________________________________________________________________________________________________________________
Good_key:
	pop bx
	mov cx, 16
	mov di, offset Key
Transform_to_bytes:
	mov al, byte ptr ds:[bx];wez char
	cmp al, 64				;porownuje z 64, na lewo mniejsze od 58 sa cyfry a na prawo litery a-f
	jg Transform_letter		;jak wieksze to literka
	sub al, 48				;z '0' zrobi 0 0000b ,... '9' 9 : 0101b
	jmp Transform_to_bytes_aftersub
Transform_letter:
	sub al, 87				;z 'a' zrobi 10 0110b ,... 'f' 15 : 1111b, -87 bo a ma w ascii 97
Transform_to_bytes_aftersub:
	shl al, 1
	shl al, 1
	shl al, 1
	shl al, 1				;mnozymy przez 2^4 = 16
	mov ah, al				;zapisujemy starsze, a potem dodamy mlodsze 4bity
	inc bx
;teraz mlodsze 4 bity:
	mov al, byte ptr ds:[bx]
	cmp al, 64
	jg Transform_letter_second
	sub al, 48
	jmp Transform_to_bytes_aftersub_second
Transform_letter_second:
	sub al, 87
Transform_to_bytes_aftersub_second:
	add al, ah				;dodajemy starsze i zapisujemy
	mov byte ptr ds:[di], al
	inc di
	inc bx
	loop Transform_to_bytes
;____________________________________________________________________________________________________________________
;klucz zapisany w postaci binarnej w 16 bajtach w zmiennej Key
;Teraz wypelnienie tablicy
;____________________________________________________________________________________________________________________
	cmp byte ptr ds:[Mod1], 1
	je Reversed_Mode		;jak nie jest 1 to znaczy ze nie zmieniamy kolumn z rzedami
	mov dl, 4
	mov dh, 8				;w dx, wspolrzedne po ktorych sie przemieszczamy, dl zawiera rzedy, a dh kolumny, wartosci zaleznie 0-8, 0-16
	jmp Mode_done
Reversed_Mode:
	mov byte ptr ds:[Rows], 17
	mov dl, 8
	mov byte ptr ds:[Cols], 9
	mov dh, 4
Mode_done:
	mov bx, offset Ramka	;bedziemy sie odnosic do elementu poprzez ds:[bx+di]
	mov di, 76				;dl*Cols+dh
	mov si, offset Key		;stad sprawdzamy bity
	xor ah, ah
	mov cx, 16
Key_full_bytes:
	push cx
	mov al, byte ptr ds:[si]
	mov cx, 4
		Key_byte_pars:
			shr al, 1
			jc Par_right_one
			;_0 lewo
				cmp dh, 0
				je Par_right_done
				dec dh
				dec di
				jmp Par_right_done
			Par_right_one:
			;_1 prawo
				inc dh
				cmp dh, byte ptr ds:[Cols]
				je Par_right_done_dec
				inc di
				jmp Par_right_done
			Par_right_done_dec:
				dec dh
			Par_right_done:
			
			
			shr al, 1
			jc Par_left_one
			;0_ gora
				cmp dl, 0
				je Par_left_done
				dec dl
				push ax
				mov al, byte ptr ds:[Cols]
				sub di, ax
				pop ax
				jmp Par_left_done
			Par_left_one:
			;1_ dol
				inc dl
				cmp dl, byte ptr ds:[Rows]
				je Par_left_done_dec
				push ax
				mov al, byte ptr ds:[Cols]
				add di, ax
				pop ax
				jmp Par_left_done
			Par_left_done_dec:
				dec dl
			Par_left_done:
			inc byte ptr ds:[bx+di]
			loop Key_byte_pars
	inc si
	pop cx
	loop Key_full_bytes
	mov bp, di
;____________________________________________________________________________________________________________________
;pokazanie ramki
;____________________________________________________________________________________________________________________
	mov bx, offset Ramka
	
		PUTCHA "+"
		mov cl, byte ptr ds:[Cols]
		Ramka_col_pre:
			PUTCHA "-"
			loop Ramka_col_pre
		PUTCHA "+"
		PUTSTR Newline
	xor ah, ah
	xor si, si
	mov di, offset AsciiTab
	mov cl, byte ptr ds:[Rows]
Ramka_row:
	push cx
	PUTCHA "|"
	mov cl, byte ptr ds:[Cols]
	Ramka_col:
		mov al, ds:[bx]
		cmp al, 14
		jg Put_dash
		add di, ax
		PUTCHA byte ptr ds:[di]
		sub di, ax
		jmp Ramka_put_done
		Put_dash:
			PUTCHA "^"
		Ramka_put_done:
		cmp si, 76
		jne Ramka_not_start
			PUTCHA 8h
			PUTCHA "S"
		Ramka_not_start:
		cmp si, bp
		jne Ramka_not_end
			PUTCHA 8h
			PUTCHA "E"
		Ramka_not_end:
		inc bx
		inc si
		loop Ramka_col
	PUTCHA "|"
	PUTSTR Newline
	pop cx
	loop Ramka_row
	
		PUTCHA "+"
		mov cl, byte ptr ds:[Cols]
		Ramka_col_post:
			PUTCHA "-"
			loop Ramka_col_post
		PUTCHA "+"
		PUTSTR Newline
	;jeszcze S i E
	mov bx, offset Ramka
	
Endonifikacja:
;koniec
	mov ah,4ch 
	int 21h
	
	
;____________________________________________________________________________________________________________________
;
;PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY 
;____________________________________________________________________________________________________________________
GetArgSize:			;w bx mamy wskaznik na P_Args od arga ktorego chcemy dlugosc do al
	add bx,2
	mov al,ds:[bx]
	sub bx,2
	sub al,ds:[bx]
	dec al
	ret
Get_args_again:
	push ax
	push cx
	push bx
	push si
	push di
	push dx
	mov di, offset Args		;destination index wskazuje teraz na poczatek moich argow
	mov bx, offset P_Args	;bx bedzie wskazywal kolejno na !!!DW!!! P_Argsy, czyli co 2
	xor bp, bp				;policzymy tu ilosc elementow
	jmp If_No_Arg_White		;parser od pewnego momentu ktory konczy sie ret stad brak reta
Parser:
;____________________________________________________________________________________________________________________
;PARSER
;zwraca w Args tak: $Arg1%Arg2$Arg3$Arg4$
;a P_Args      tak: 1^   2^   3^   4^   5^
;a w bp ilosc argumentow
;____________________________________________________________________________________________________________________
	jmp Skip_Pars_Proc	;przeskakuje procedury
	;tutaj mam procedury ktorych uzywam w parserze

	;wypisuje No_arg_err
	No_Arg_Msg:
	;przez to ze w ds nie mamy danych a stringa w int sie wypisuje z ds to trzeba przezucic chwilowo es do ds
		push ax
		push dx
		push ds
		mov ax,es
		mov ds,ax
		lea ax,No_Arg_err
		mov dx,ax
		mov ah,09h
		int 21h
		pop ds
		pop dx
		pop ax
		ret
	Getchar:
	;wrzuca do al input
		push dx
		mov ah, 1h
		int 21h
		pop dx
		ret
	Getchar_noshow:
	;wrzuca do al input bez pokazania
		push dx
		mov ah, 7h
		int 21h
		pop dx
		ret
	Makearg:
	;gdy jest ch = 0, dajemy ch=1 i wrzucamy na nasza tablice wskaznikow P_Args po bx-ie i dodajemy ilosc argumentow w bp
		mov ch, 1
		inc bp
		mov word ptr es:[bx],di
		add bx, 2				;dodajemy do bx 2 bo offsety na poczatki argumentow to wordy a nie byte
		ret
	Skip_Pars_Proc:
	;____________________________________________________________________________________________________________________
	;Tutaj zaczyna sie poczatek wykonywania operacji parsera
	;____________________________________________________________________________________________________________________
	push ax
	push cx
	push bx
	push si
	push di
	push dx
	;____________________________________________________________________________________________________________________
	;OKRESLENIE SEGMENTOW I ARGUMENTOW OD KTORYCH ZACZYNAMY
	;____________________________________________________________________________________________________________________
		mov ax, seg Args		;analogicznie mov ax, dane1 by dzialalo
		mov es, ax				;teraz moge zmieniac wartosci np majac w di offset argi , es:bx
		mov si, [081h]			;source destination czyli tam gdzie na poczatku sa argv, sam offset
		mov di, offset Args		;destination index wskazuje teraz na poczatek moich argow
		mov bx, offset P_Args	;bx bedzie wskazywal kolejno na !!!DW!!! P_Argsy, czyli co 2
		mov cl, byte ptr ds:[080h];w cl teraz jest ilosc znakow jakie uzytkownik wpisal
		xor ch, ch				;jak ch to 0 to znaczy ze mamy pierwszy element
		xor bp, bp				;policzymy tu ilosc elementow
		cmp cl, 0
		jne Check_Source		;gdy mamy przynajmniej jeden char
	;____________________________________________________________________________________________________________________
	;ponizszy kod bierze parsuje argumenty do Args poprzez es:[di] i wskazniki do nich do P_Args poprzez word ptr es:[bx], dodatkowo w  bp zwroci ilosc argumentow
	;uzywamy ax,dx,cx,ds,es,bp odpowiednio przygotowanych
	;uzywamy No_Arg_Msg,Getchar_noshow,

	;1. Zjada wszystkie spacje taby entere do pierwszego nie bialego znaku

	If_No_Arg:;tutaj z wiadomoscia error
		call No_Arg_Msg;zawsze wywalamy error gdy nic nie wpisze i da enter
	If_No_Arg_White:;tutaj bez wiadomosci error
		call Getchar_noshow		;zwraca wczytane do al
		IFWHITEJUMPTO If_No_Arg_White;gdy spacja tab
		
		;gdy enter lub backspace to tez wracamy
		cmp al, 8h
		je If_No_Arg_White
		cmp al, 0dh
		je If_No_Arg
	;teraz w al napewno jest nie bialy znak

	;2. Teraz enter konczy i zapisujemy nie white

	No_Arg_Put:
		cmp ch, 0
		je Makearg_not_si		;teraz jak byl pierwszy char nowego argumentu mamy go wrzuconego do naszych P_Argsow
	After_FirstCh_not_si:
		mov es:[di], al
		PUTCHA al
		inc di
		Bspace_eat:
			call Getchar_noshow
			cmp al, 8h
			jne A_F_n_s_backspace
			PUTCHA 8h
			PUTCHA ' '
			PUTCHA 8h
			dec di
			sub bx,2			;musimy pamietac ze bx wskazuje na kolejny argument, stad musimy sprawdzic poprzedni
			cmp word Ptr es:[bx], di
			pushf				;potrzebne bo operacja add zmienia flage
			add bx,2
			popf
			jne Bspace_eat
			
			;jesli poczatek arga to odejmujemy od di jeszcze 1 bo argi przed poczatkiem maja dolara i odejmujemy od bx 2 bo odejmujemy 1 argument
			dec di
			sub bx,2
			dec bp
			xor ch,ch
			
			;teraz jesli di to poczatek wszystkich argow to resetujemy cale wczytywanie spowrotem do 1. tylko be komunikatu
			cmp di, offset Args
			je If_No_Arg_White
			PUTCHA 8h			;bo wchodzimy na miejsce spacji i damy znowu spacje wiec przesunmy karetke o 1 w lewo Backspacem
			jmp Make_FirstCh_and_Whitekill
	A_F_n_s_backspace:
		IFWHITEJUMPTO Make_FirstCh_and_Whitekill
	FirstCh_Made:
		cmp al, 0dh
		je Pars_End				;gdy enter to na 100% mamy jakies argumenty
		jmp No_Arg_Put			;teraz nie enter ani biale wiec mozemy al dac do naszego di
	Make_FirstCh_and_Whitekill:
		PUTCHA ' '
		xor ch, ch
	FirstCh_Set_Now:
		call Getchar_noshow
		cmp al, 8h
		jne F_s_n_backspace
		PUTCHA 8h
		PUTCHA ' '
		PUTCHA 8h
		inc ch
		jmp Bspace_eat
	F_s_n_backspace:
		IFWHITEJUMPTO FirstCh_Set_Now
		jmp FirstCh_Made
	Makearg_not_si:				;wczesniej uzyte dodawanie wskaznika na argument
		push ax
		mov al, "$"
		mov es:[di], al
		inc di
		pop ax
		call Makearg			;kolejne wskazniki
		jmp After_FirstCh_not_si
	;____________________________________________________________________________________________________________________
	Check_Source:
		mov al, ds:[si]			;sprawdzimy czy nie ma spacji lub tabow
		IFWHITEJUMPTO Kill_White
	Arg_Put:
		cmp ch, 0
		je Makearg_si
	After_FirstCh_si:
		mov es:[di], al
		inc di
		inc si					;zawsze przy inc si, musi byc dec cl
		dec cl
		cmp cl,0
		je Pars_End
		mov al, ds:[si]
		IFWHITEJUMPTO FirstCh_Kill_White
		jmp Arg_Put
	FirstCh_Kill_White:
		xor ch, ch
	Kill_White:
		inc si					;zawsze przy inc si, musi byc dec ch
		dec cl
		cmp cl,0
		je Pars_End
		mov al, ds:[si]
		IFWHITEJUMPTO Kill_White
		jmp Arg_Put
	Makearg_si:
		push ax
		mov al, "$"
		mov es:[di], al
		inc di
		pop ax
		call Makearg
		jmp After_FirstCh_si
	;____________________________________________________________________________________________________________________
	Pars_End:
		mov ah, byte ptr "$"
		mov es:[di], ah
		inc di
		mov word ptr es:[bx],di;potrzeba jeszcze jednego wskaznika na koniec zeby liczyc dlugosc argow
	;____________________________________________________________________________________________________________________
	pop dx
	pop di
	pop si
	pop bx
	pop cx
	pop ax
	ret
;____________________________________________________________________________________________________________________



;____________________________________________________________________________________________________________________

code ends
;____________________________________________________________________________________________________________________

stos1 segment stack
			dw 200h dup(?)	;czyli stos ma 512 komorek po 2bajty czyli 1kb + wstosu
	wstosu	dw ?			;? powoduje ze wie ze tam ma byc slowo czyli 2bajty ale nie wazne co narazie wiec ?
stos1 ends
;____________________________________________________________________________________________________________________

end start1
;____________________________________________________________________________________________________________________
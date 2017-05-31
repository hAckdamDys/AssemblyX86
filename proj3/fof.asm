.387
;PROGRAM POWINIEN ZAPAMIETYWAC POZYCJE ZMIENNOPRZECINKOWO BO r1m1r1m1 nie działa dobrze
;i pi/180 raz obliczyc reszta spoko
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
;makra:
SHOW macro Source
	push dx
	push ax
	lea dx,Source
	mov ah,9h
	int 21h
	pop ax
	pop dx
endm
IFWHITEJUMPTO macro Dest	;gdy przenosimy parser, potrzebujemy to macro + 3 zmienne Args, P_Args, errNoArg i zmienic Endonifikacja na koniec programu
	cmp al, 27				;escape
	je Endonifikacja		;tutaj zapisujemy jak sie nazywa koniec
	cmp al, 20h
	je Dest
	cmp al, 09h
	je Dest
endm
MULTENDX macro
	push ax
	mov ax, dx
	sal ax, 1
	sal ax, 1
	sal ax, 1;*8
	push ax
	mov ax, dx
	sal ax, 1;*2
	mov dx, ax
	pop ax
	add dx, ax;dx=dx*10
	pop ax
endm
COPY macro Dest, Source		;choose handle, wybieramy uchwyt
	push ax
	mov ax, ds:[Source]
	mov ds:[Dest], ax
	pop ax
endm
;____________________________________________________________________________________________________________________

BLACK equ 0
BLUE equ 1
GREEN equ 2
CYAN equ 3
RED equ 4
PURPLE equ 5
ORANGE equ 6

dane1 segment

;parser:
Args		db 100h dup(?)			;256 bajtow na 256 znakow
P_Args		dw 40h dup(?)			;64 adresow na argi wazne to slowa DW!!!! gdy chcemy kolejny to albo incx2 albo add 2
errNoArg	db "Brak argumentow $"

decompressInstead db 0
fileName	dw ?
fileHandle	dw 44					;tutaj zapiszemy uchwyt do pliku z ktorego bierzemy, 44 bo takiego uchwytu nie ma i wtedy wywali blad
fileBuffor 	db 4010h dup (?)		;do tego bufora bedziemy odczytywac z pliku
portionSize	dw 4000h				;po tyle porcjujemy
justDollar 	db "$"					;gdyby podczas wypisywania buffora bylo cos nie tak

isPenDown	db 1
brushColor	db ORANGE
bgColor		db BLACK
x1			dw 160	;320/2 
y1			dw 100	;200/2 
dyy			dw ?	;sinalfa
deg			dw 270	;30.0: cos 0.8660, sin 0.5000
f1000		dq 1000.0
fi180		dw 180
fi90		dw 90
fi320		dw 320
delta		dw ?	;320int(sin)+int(cos), tyle dodajemy w pętli do di bLen razy
bLen		dw 15	;dlugosc
point 		dw 160+100*320

testNum		dw 534

newLine		db 0ah,0dh,"$"

errTooLittleArgs db "Za malo argumentow $"
errTooManyArgs db "Za duzo argumentow $"
errFileLoad	db "Niestety nie udalo sie otworzyc pliku :($"
errCantRead	db "Blad odczytu pliku :($"
errBadCommands db "Zle komendy w pliku $"
errClosingBadHandle	db ".$"			;krotki komentarz bo czesto wystepuje gdy sa inne bledy i jest wtedy nieistotny bo nie bylo czego zamykac

dane1 ends
;____________________________________________________________________________________________________________________


code segment

start1:
;____________________________________________________________________________________________________________________
;stos_init:
	mov ax, stos1
	mov	ss, ax
	mov	sp, offset wstosu
;parser:
	call Parser
;ds jako segment w ktorym mam dane
	mov ax, dane1
	mov ds, ax
;____________________________________________________________________________________________________________________
;main:
	call CheckArgs
	call MakeFileName
	call OpenFile
	call EnterGraphics
	finit
	call ReadFile;czyta plik i wywoluje odpowiednie funkcje
;____________________________________________________________________________________________________________________
;koniec
Endonifikacja:
	call CzekajNaZnak
	call EnterTextMode
	call CloseFile
	mov ah, 4ch
	int 21h
	
	
;____________________________________________________________________________________________________________________
;
;PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY 
;____________________________________________________________________________________________________________________
EnterGraphics:
	push ax
	call CzekajNaZnak	;____________________________________________________________________________________________________________________ usunąć po usunięciu wypisywania podczas readfile
	mov ax, 13h ;tryb graficzny 320x200 punktów / 256 kolorów, ah 0, al 13h 
	int 10h
	mov ax, 0a000h
	mov es, ax	;w es mamy naszą mape do wypelniania kolorami
	mov al, ds:[bgColor]
	call Fill
	call CzekajNaZnak
	pop ax
	ret
CzekajNaZnak:
	push ax
	xor ax, ax
	int 16h	;get keystroke
	pop ax
	ret
Fill:				;musi miec kolor w al
	push cx
	push di
	mov ah, al
	mov cx, 320*200/2 ;wypelniamy tyle podwojnych bajtow
	mov di, 0 		;rejestr es:di jest wskaznikiem na cel, do ktorego chcemy cos zapisywac uzywajac stosb
	cld 			;wskaznik flagi, ze jedziemy rosnaco  clear direction, podobnie jak clc clear carry
	rep stosw 		;zapisz slowo ax w es:di i zwiększa (dla df=0) DI, powtarzaj to CX razy 
	pop di
	pop cx
	ret
CurveLine:;potrzebuje x1, y1, x1+320y1=point
	push dx
	push ax
	push cx
	push bx
	push di
	mov ax, word ptr ds:[point]
	mov di, ax
	fild word ptr ds:[deg]	;st(0)=deg
	fldpi				;pi, deg
	fmulp st(1), st(0)	;pi*deg
	fild word ptr ds:[fi180];180, pi deg
	fdivp st(1), st(0)		;pi deg/ 180
	fsincos				;cos, sin, pideg/180
	fxch st(1)			;bo y po sin a x po cos, teraz: sin,cos,pideg/180
	mov al, byte ptr ds:[isPenDown]
	cmp al, 1
	je DrawCurveLine
	;trzeba zrobic nsin i ncos i bez rysowania zupdejtowac x1 y1
	fild word ptr ds:[bLen];n,sin,cos,..
	fxch st(2);cos,sin,n, pideg/180
	fmul st(0), st(2);ncos, sin, n, pideg/180
	fxch st(1)
	fmul st(0), st(2);nsin, ncos, n, pideg/180
	jmp TrigLoopEnd
	DrawCurveLine:
	fldz
	fldz			;dwa zera tutaj bedziemy dodawac nsin i ncos
	fild word ptr ds:[fi320];320,...
	fxch st(5);swap 320,pideg/180
	fstp st(0);przesun o 1 w lewo ucinajac kąt już niepotrzebny: sin,cos,sin,cos,st(4)=320
	mov cx, word ptr ds:[bLen];dlugosc lini
	cmp cx, 0
	je TrigLoopEnd
	TrigLoop:
		;nsin,ncos,sin,cos,320
		fadd st(0),st(2)	;st(0)+=sin, wielokrotnosci sinusa

		fist word ptr ds:[dyy];dyy=int(nsin)
		fild word ptr ds:[dyy];int(nsin),nsin,ncos,sin,cos, st(5)=320

		fmul st(0), st(5);320int(dyy),nsin, ncos, sin, cos, 320

		fxch st(2);ncos,nsin,320(dyy),sin,st(4)=cos,320=st(5)
		fadd st(0), st(4);kolejna wielokrotnosc cosinusa st(0)+=cos

		fld st(0);powielamy
		fxch st(3);odmieniamy z 320(dyy)
		;mamy teraz 320(dyy),dxx,nsin,ncos,sin,cos,320=st(6)
		faddp st(1), st(0);320(dyy)+dxx,nsin,ncos...
		fistp word ptr ds:[delta];nsin,ncos,...
		
		mov bx, word ptr ds:[delta]
		mov al, byte ptr ds:[brushColor]
		mov byte ptr es:[bx+di], al
		loop TrigLoop
	TrigLoopEnd:
	;x1+=ncos, if 0<x1<320 good, y1+=nsin, if 0<y1<200 good, point=320y1+x1:
	fild word ptr ds:[y1]
	faddp st(1), st(0);y1+nsin,ncos,sin,cos,320
	fistp word ptr ds:[y1];ncos,sin,cos,320, y1+=nsin
	;jak mniejsze od 0 to mamy unsigned czyli wieksze niz 200 wychwyci
	mov ax, word ptr ds:[y1]
	cmp ax, 200
	xor dx, dx
	jg FileBadArgs
	push ax
	mov ax, 320
	mov bx, ax
	pop ax
	mul bx;dx:ax = dx * bx, wiemy ze miesci sie w ax calosc
	mov dx, ax;zapisujemy w dx potem to dodamy
	fild word ptr ds:[x1]
	faddp st(1), st(0);x1+ncos,sin,cos,320
	fistp word ptr ds:[x1];sin,cos,320
	fstp st(0)
	fstp st(0)
	fstp st(0);pusty stack
	mov ax, word ptr ds:[x1]
	;jak mniejsze od 0 to mamy unsigned czyli wieksze niz 320 wychwyci
	cmp ax, 320
	jg FileBadArgs
	add ax, dx;ax=x1+320y1 nowe
	mov word ptr ds:[point], ax
	
	pop di
	pop bx
	pop cx
	pop ax
	pop dx
	ret
EnterTextMode:
	push ax
	mov ax, 3	;tryb tekstowy , ah 0, al 13h
	int 10h 	;powrot do trybu tekstowego
	pop ax
	ret
FileBadArgs:
	SHOW errBadCommands
	jmp Endonifikacja
;____________________________________________________________________________________________________________________
ReadFile:
	push ax
	push dx
	push bx
	push cx
	mov cx, ds:[portionSize];ile bajtow czytamy, wielkosc porcji
	lea dx, fileBuffor		;gdzie czytamy
	mov bx, ds:[fileHandle]
	mov ah, 03fh			;read file
	int 21h
	jc FileBadRead
	;udalo sie odczytac plik tam gdzie ustawilismy dx i ilosc bajtow przeczytanych dostajemy w ax
	mov cx, ax				;w cx zapamietujemy ile bajtow dostalismy
	mov bx, dx				;bx wskazuje na poczatek bajtow ktore otrzymalismy
	ReadFileLoop:
		mov al, ds:[bx]
		cmp al, ' '
		je RFLend
		cmp al, 0ah
		je RFLend
		cmp al, 09h
		je RFLend
		cmp al, 0
		je RFLend
		cmp al, 0dh
		je RFLend
		cmp al, 'r'
		jne RFLNotRotate
		;do ax potrzeba liczby stopni
		call MakeAxNum
		call Rfun
		jmp RFLend
		RFLNotRotate:
		cmp al, 'u'
		je Ufun
		cmp al, 'd'
		je Dfun
		cmp al, 'm'
		jne FileBadArgs
		;do ax potrzeba liczby dlugosci
		call MakeAxNum
		call Mfun
		RFLend:
		inc bx
		loop ReadFileLoop
	pop cx
	pop bx
	pop dx
	pop ax
	ret
MakeAxNum:;dx jest zpushowany i zpopwany miedzy wykonywaniem tego
	xor dx, dx
	xor ah, ah ;" 30"
	MakeAxNumLoop:
	dec cx
	cmp cx, 0
	je MakeAxNumEndCxFix
	inc bx
	mov al, byte ptr ds:[bx]
	cmp al, '$'
	je MakeAxNumEnd
	cmp al, 0
	je MakeAxNumEnd
	cmp al, 0ah
	je MakeAxNumEnd
	cmp al, 0dh
	je MakeAxNumEnd
	cmp al, ' '
	je MakeAxNumLoop
	cmp al, 09
	je MakeAxNumLoop
	cmp al, '0'
	jl FileBadArgs
	cmp al, '9'
	jg FileBadArgs
	sub al, '0'
	MULTENDX;dx=dx*10, ax bez zmian
	add dl, al;dx+=al
	adc dh, 0;jak za duzo
	jmp MakeAxNumLoop
	MakeAxNumEndCxFix:
	inc cx
	MakeAxNumEnd:
	mov ax, dx
	ret
Ufun:
	xor al, al;mov al, 0
	mov byte ptr ds:[isPenDown], al	
	jmp RFLend
Dfun:
	mov al, 1
	mov byte ptr ds:[isPenDown], al
	jmp RFLend
FileBadRead:				;error
	SHOW errCantRead
	jmp Endonifikacja
Rfun:
	push dx
	push bx
	;w ax mamy kąt ile chcemy dodać
	add ax, word ptr ds:[deg]
	;reszta z dzielenia przez 360 ax:
	push ax
	mov ax, 360
	mov bx, ax
	pop ax
	xor dx, dx
	div bx;dx to reszta z dzielenia dx:ax przez bx
	mov ax, dx
	mov word ptr ds:[deg], ax
	pop bx
	pop dx
	ret
Mfun:
	;w ax mamy dlugosc
	mov word ptr ds:[bLen], ax
	call CurveLine
	ret
CheckArgs:
	cmp bp, 1
	je GoodArgs
	cmp bp, 0
	je TooLittleArgs
	SHOW errTooManyArgs
	jmp Endonifikacja
	TooLittleArgs:
		SHOW errTooLittleArgs
		jmp Endonifikacja
	GoodArgs:
	ret
MakeFileName:		;te Make'y zamienia nam $ w argsach na 0
	push bx
	push ax
	lea bx, P_Args
	COPY fileName, bx
	call GetArgSize	;w al mamy dlugosc
	xor ah, ah
	mov bx, ds:[bx]
	add bx, ax
	mov al, 0		;zamiana tego dolara na zero
	mov ds:[bx], al	;
	pop ax
	pop bx
	ret
OpenFile:
	push ax
	push dx
	xor al, al
	mov dx, ds:[fileName]
	mov ah, 03dh			;open file
	int 21h
	jc FileLoadError
	;jesli nie bylo bledow:
	mov ds:[fileHandle], ax
	pop dx
	pop ax
	ret
	FileLoadError:
		SHOW errFileLoad
		;sub sp, 6			;bo wykonalismy call i 2 pushe wordów
		jmp Endonifikacja
CloseFile:
	push ax
	push bx
	mov bx, ds:[fileHandle]
	mov ah, 03eh			;close file
	int 21h
	jnc CloseFileEnd
	SHOW errClosingBadHandle
CloseFileEnd:
	pop bx
	pop ax
	ret
	
GetArgSize:			;w bx mamy wskaznik na P_Args od arga ktorego chcemy dlugosc do al
	add bx,2
	mov al,ds:[bx]
	sub bx,2
	sub al,ds:[bx]
	dec al
	ret
;____________________________________________________________________________________________________________________
;PARSER
;zwraca w Args tak: $Arg1%Arg2$Arg3$Arg4$
;a P_Args      tak: 1^   2^   3^   4^   5^
;a w bp ilosc argumentow
;____________________________________________________________________________________________________________________
Parser:
	push ax
	push cx
	push bx
	push si
	push di
	push dx
	;____________________________________________________________________________________________________________________
	;OKRESLENIE SEGMENTOW I ARGUMENTOW OD KTORYCH ZACZYNAMY
	;____________________________________________________________________________________________________________________
	mov ax, seg Args			;analogicznie mov ax, dane1 by dzialalo
	mov es, ax					;teraz moge zmieniac wartosci np majac w di offset argi , es:bx
	mov si, [081h]				;source destination czyli tam gdzie na poczatku sa argv, sam offset
	mov di, offset Args			;destination index wskazuje teraz na poczatek moich argow
	mov bx, offset P_Args		;bx bedzie wskazywal kolejno na !!!DW!!! P_Argsy, czyli co 2
	mov cl, byte ptr ds:[080h]	;w cl teraz jest ilosc znakow jakie uzytkownik wpisal
	xor ch, ch					;jak ch to 0 to znaczy ze mamy pierwszy element
	xor bp, bp					;policzymy tu ilosc elementow
	cmp cl, 0
	jne Check_Source			;gdy mamy przynajmniej jeden char
	;____________________________________________________________________________________________________________________
	;gdy nie ma argumentow error i koniec programu
	mov ax, es
	mov ds, ax
	SHOW errNoArg
	;sub sp, 14
	jmp Endonifikacja
	;ponizszy kod bierze parsuje argumenty do Args poprzez es:[di] i wskazniki do nich do P_Args poprzez word ptr es:[bx], dodatkowo w  bp zwroci ilosc argumentow
	;uzywamy ax,dx,cx,ds,es,bp odpowiednio przygotowanych
	;uzywamy No_Arg_Msg,Getchar_noshow,
	;____________________________________________________________________________________________________________________
	Check_Source:
		mov al, ds:[si]			;sprawdzimy czy nie ma spacji lub tabow
		IFWHITEJUMPTO Kill_White
	Arg_Put:
		cmp ch, 0
		je Makearg
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
		cmp cl, 0
		je Pars_End
		mov al, ds:[si]
		IFWHITEJUMPTO Kill_White
		jmp Arg_Put
	Makearg:
		push ax
		mov al, "$"
		mov es:[di], al
		inc di
		pop ax
		;gdy jest ch = 0, dajemy ch=1 i wrzucamy na nasza tablice wskaznikow P_Args po bx i dodajemy ilosc argumentow w bp
		mov ch, 1
		inc bp
		mov word ptr es:[bx], di
		add bx, 2				;dodajemy do bx 2 bo offsety na poczatki argumentow to wordy a nie byte
		jmp After_FirstCh_si
	;____________________________________________________________________________________________________________________
	Pars_End:
		mov ah, byte ptr "$"
		mov es:[di], ah
		inc di
		mov word ptr es:[bx], di;potrzeba jeszcze jednego wskaznika na koniec zeby liczyc dlugosc argow
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
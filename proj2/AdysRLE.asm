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
TAKEPORTION macro
	mov cx, ds:[portionSize];ile bajtow czytamy, wielkosc porcji
	lea dx, fileBuffor		;gdzie czytamy
	mov bx, ds:[fileHandleR]
	mov ah, 03fh			;read file
	int 21h
	jc FileBadRead
	;udalo sie odczytac plik tam gdzie ustawilismy dx i ilosc bajtow przeczytanych dostajemy w ax
	cmp ax, 0
	je EndPortioning		;wczytujemy porcje do momentu gdy mamy zero bajtow
	mov cx, ax				;w cx zapamietujemy ile bajtow dostalismy
	mov bx, dx				;bx wskazuje na poczatek bajtow ktore otrzymalismy
	mov ah, ds:[bx]			;bierzemy znak do ah
endm
IFWHITEJUMPTO macro Dest	;gdy przenosimy parser, potrzebujemy to macro + 3 zmienne Args, P_Args, errNoArg i zmienic Endonifikacja na koniec programu
	cmp al, 27				;escape
	je Endonifikacja		;tutaj zapisujemy jak sie nazywa koniec
	cmp al, 20h
	je Dest
	cmp al, 09h
	je Dest
endm
COPY macro Dest, Source		;choose handle, wybieramy uchwyt
	push ax
	mov ax, ds:[Source]
	mov ds:[Dest], ax
	pop ax
endm
;____________________________________________________________________________________________________________________

dane1 segment

;parser:
Args		db 100h dup(?)			;256 bajtow na 256 znakow
P_Args		dw 40h dup(?)			;64 adresow na argi wazne to slowa DW!!!! gdy chcemy kolejny to albo incx2 albo add 2

portionSize	dw 4000h				;po tyle porcjujemy
decompressInstead db 0
fileNameR	dw ?
fileNameS	dw ?
fileHandleR	dw 44					;tutaj zapiszemy uchwyt do pliku z ktorego bierzemy, 44 bo takiego uchwytu nie ma i wtedy wywali blad
fileHandleS	dw 44					;tutaj zapiszemy uchwyt do pliku do ktorego zapisujemy
fileHandle	dw 44					;uchwyt ktorego aktualnie uzywamy
fileBuffor 	db 4010h dup (?)		;do tego bufora bedziemy odczytywac z pliku, musi byc wieksze niz wielkosc porcji
fileBufforS	db 4010h dup (?)		;do tego zapisujemy 
justDollar 	db "$"					;gdyby podczas wypisywania buffora bylo cos nie tak

errNoArg	db "Brak argumentow $"
errTooLittleArgs db "Za malo argumentow $"
errTooManyArgs db "Za duzo argumentow $"
errFileLoad	db "Niestety nie udalo sie otworzyc pliku :($"
errCantRead	db "Blad odczytu pliku :($"
errCantWrite db "Blad zapisu do pliku :($"
errClosingBadHandle	db ".$"			;krotki komentarz bo czesto wystepuje gdy sa inne bledy i jest wtedy nieistotny bo nie bylo czego zamykac
errFileExist db "Plik istnieje, nadpisac?(enter = tak, reszta = nie):",10,13,'$'
errFileCreate db "Nie udalo sie utworzyc pliku$"
errBadOption db "Jedyna opcja to -d$"

napisKoniecPorcji db ":$"

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
	call MakeFileNames
	call OpenFileR
	COPY fileHandleR, fileHandle	;OpenFileR i OpenFileS korzystaja obie z OpenFile stad potem jeszcze odpowiednio kopiujemy z filehandla
	call CreateFile
	call OpenFileS
	COPY fileHandleS, fileHandle
	call Compression
;____________________________________________________________________________________________________________________
;koniec
Endonifikacja:
	COPY fileHandle, fileHandleR
	call CloseFile
	COPY fileHandle, fileHandleS
	call CloseFile
	mov ah, 4ch
	int 21h
	
	
;____________________________________________________________________________________________________________________
;
;PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY PROCEDURY 
;____________________________________________________________________________________________________________________
CheckArgs:
	cmp bp, 2
	je GoodArgs
	;jesli jeden lub wiecej niz 2
	cmp bp, 1
	je TooLittleArgs
	cmp bp, 3
	je TogleDecompressionOn
	SHOW errTooManyArgs
	jmp Endonifikacja
	TooLittleArgs:
		SHOW errTooLittleArgs
		jmp Endonifikacja
	BadOption:
		SHOW errBadOption
		jmp Endonifikacja
	TogleDecompressionOn:
		push bx
		push ax
		lea bx, Args
		inc bx			;bo pierwszy jest $ po parsowaniu
		mov al, ds:[bx]	;bo bajt
		cmp al, 2dh		;"-"
		jne BadOption
		inc bx
		mov al, ds:[bx]
		cmp al, 64h		;"d"
		jne BadOption
		inc ds:[decompressInstead]	;ustawienie dekompresji
		pop ax
		pop bx
	GoodArgs:
	ret
MakeFileNames:		;te Make'y zamienia nam $ w argsach na 0
	push bx
	push ax
	mov al, ds:[decompressInstead]
	cmp al, 0
	lea bx, P_Args
	je MakeFileName_normal
	;w przypadku dekompresji:
	add bx, 2
MakeFileName_normal:
	COPY fileNameR, bx
	call GetArgSize		;w al mamy dlugosc
	push bx
	xor ah, ah
	mov bx, ds:[bx]
	add bx, ax
	mov al, 0
	mov ds:[bx], al
	pop bx
	add bx, 2
	COPY fileNameS, bx
	call GetArgSize
	mov bx, ds:[bx]
	add bx, ax
	mov al, 0
	mov ds:[bx], al
	pop ax
	pop bx
	ret
CreateFile:
	push dx
	push ax
	push cx
	mov dx, ds:[fileNameS]
	xor cx, cx				;cx = file attribute, 0 -> read-only file
	mov ah, 05bh			;create file ktory wywala error gdy istnieje
	int 21h
	jc FileCreateExist
	;jesli nie bylo bledow:
CreateFileEnd:
	pop cx
	pop ax
	pop dx
	ret
	FileCreateExist:
		SHOW errFileExist
		mov ah, 07h
		int 21h
		cmp al, 0dh			;jak enter do tworzymy na hama, jak nie to wychodzimy
		jne Endonifikacja
		mov ah, 03ch		;create file nie wywalajacy errora
		int 21h
		jnc CreateFileEnd
		SHOW errFileCreate
		jmp Endonifikacja
OpenFile:
	mov ah, 03dh			;open file
	int 21h
	jc FileLoadError
	;jesli nie bylo bledow:
	mov ds:[fileHandle], ax
	ret
	FileLoadError:
		SHOW errFileLoad
		;sub sp, 6			;bo wykonalismy call i 2 pushe wordów
		jmp Endonifikacja
OpenFileR:
	push ax
	push dx
	xor al, al
	mov dx, ds:[fileNameR]
	call OpenFile
	pop dx
	pop ax
	ret
OpenFileS:
	push ax
	push dx
	mov dx, ds:[fileNameS]
	mov al, 1				;xor to 0 to read, 1 to write, a 2 to w i r
	call OpenFile
	pop dx
	pop ax
	ret
;____________________________________________________________________________________________________________________
Compression:
	push ax
	push bx
	push dx
	push cx
	push di
	push si
	;normalna kompresja:
	lea di, fileBufforS			;tutaj bedziemy zapisywac
	xor si, si					;tutaj liczymy ile bajtow juz zapisalismy do bufora
	mov al, ds:[decompressInstead]
	cmp al, 0
	jne Decomp_StartPortion
	Comp_StartPortion:
		TAKEPORTION				;makro, bo uzywamy tego samego w Decompie
		CompressBytes:
			mov al, ah
			dec cx
			cmp cx, 0
			je TakeAnotherPortion;zanim to zrobimy trzeba zapisac poprzednika
			inc bx
			mov ah, ds:[bx]
			cmp al, ah
			je ByteReapeater
			call PutByte		;zapisujemy poprzedni bajt do bufora
			jmp CompressBytes
		TakeAnotherPortion:
			call PutByte
			jmp Comp_StartPortion
		ByteReapeater:
			mov dx, cx			;ile zostalo bajtow w buferze z read
			mov cl, 2
			cmp al, 0
			je DoCompression
			dec dx						;tutaj odejmuje i sprawdzam czy moge wziac 3
			cmp dx, 0					;jak tak to wpisz te 2 ostatnie znaki i nowa partia
			jne ByteReapeater_keepGetting
			call PutByteSkipZero		;jak ostatnie 2 bajty to je wypisujemy
			call PutByteSkipZero
			jmp Comp_StartPortion
			ByteReapeater_keepGetting:	;gdy nie jest koniec bufora
			inc bx
			inc cl
			mov ah, ds:[bx]
			cmp ah, al
			je DoCompression
			;wpisz 2 znaki jak nie ma conajmniej 3 zeby robic legit kompresje
			call PutByteSkipZero
			call PutByteSkipZero
			mov cx, dx			;kontunuujemy wczytywanie
			jmp CompressBytes	;
			DoCompression:		;zero skacze odrazu tu bo jak sa 2 zera to i tak zapisujemy 0x00 0x02 0x00
			;bierzemy kolejne chary dopoki sa takie same i cl jest mniejsze od 255
			dec dx
			cmp dx, 0
			jne DoCompression_notEmpyBuf
			call ByteReapeaterPut
			jmp Comp_StartPortion
			DoCompression_notEmpyBuf:
			inc bx				;najpierw sprawdzamy kolejny 
			mov ah, ds:[bx]
			cmp ah, al
			jne ByteReapeaterEnd
			inc cl				;potem czy nie przekroczylismy
			cmp cl, 255			;gdy przekroczymy to to cos sie wali
			je ByteReapeaterEndClFix
			jmp DoCompression
			ByteReapeaterEndClFix:
				inc bx
				mov ah, ds:[bx]
				dec dx
			ByteReapeaterEnd:
				call ByteReapeaterPut
				mov cx, dx		;kont wczyt
				jmp CompressBytes;
			ByteReapeaterPut:
				mov ch, al
				mov al, 0
				call PutByteSkipZero
				mov al, cl
				call PutByteSkipZero
				mov al, ch
				call PutByteSkipZero
				ret
	PutByte:
		cmp al, 0
		jne PutByteSkipZero
		mov ds:[di], al
		inc di
		inc si
		cmp si, 4000h
		jl PutByteSkipZero
		call SavePortion
	PutByteSkipZero:
		mov ds:[di], al
		inc di
		inc si
		cmp si, 4000h
		jl EndPutByte
		call SavePortion
		EndPutByte:
		ret
	SavePortion:			;wpisywanie do pliku
		push ax
		push bx
		push cx
		push dx
		mov cx, si
		lea dx, fileBufforS
		mov bx, ds:[fileHandleS]
		mov ah, 040h
		int 21h
		jc FileBadWrite
		lea di, fileBufforS
		xor si, si
		pop dx
		pop cx
		pop bx
		pop ax
		ret
	EndPortioning:			;koniec kompresowania lub dekompresowania, uzwgledniajacy wpisanie z bufora do pliku ostatni raz
		cmp si, 0
		je EndPortioningSiDone
		call SavePortion
		EndPortioningSiDone:
		pop si
		pop di
		pop cx
		pop dx
		pop bx
		pop ax
		ret
	FileBadRead:			;errory
		SHOW errCantRead
		jmp Endonifikacja
	FileBadWrite:
		SHOW errCantWrite
		jmp Endonifikacja
;____________________________________________________________________________________________________________________
	Decomp_StartPortion:
		TAKEPORTION				;makro uzywane tez w Comp
		mov al, ah				;poprawka na al bo tutaj tylko potrzebujemy al a tam potrzebny byl poprzednik
		DecompressBytes:		;mamy w ah poprzedni bajt, musimy tylko sprawdzac czy jest zerem
			cmp al, 0
			je PutExtraBytes
			PutExtraBytesAlmostDone:
			call PutByteSkipZero
			PutExtraBytesDone:
			dec cx
			cmp cx, 0
			je Decomp_StartPortion
			inc bx
			mov al, ds:[bx]
			jmp DecompressBytes
		PutExtraBytes:			;jesli tu jestesmy to mamy albo 2 nule, wtedy wstawiam nul, albo: (nul, ile powtarzamy, jaki znak)
			dec cx				;wiemy ze nie ma konca
			inc bx
			mov al, ds:[bx]
			cmp al, 0
			je PutExtraBytesAlmostDone;almost bo po prostu ominelismy jedno zero bo byly dwa zera i to zero jeszcze wstawimy
			dec cx
			push cx				;jestesmy na drugiej pozycji w naszej trojce bajtow, ktora odpowiada za liczbe powtorzen
			mov cl, ds:[bx]		;ile razy powtarzamy
			xor ch, ch			;trzeba pamietac o wyzerowaniu H.O. od cx
			inc bx
			mov al, ds:[bx]		;noi znak
			PutExtraBytesLoop:
				call PutByteSkipZero
				loop PutExtraBytesLoop
			pop cx
			jmp PutExtraBytesDone;done bo juz nie wstawiamy nic idziemy do kolejnych bajtow
;____________________________________________________________________________________________________________________
;____________________________________________________________________________________________________________________
CloseFile:
	push ax
	push bx
	mov bx, ds:[fileHandle]
	mov ah, 03eh			;close file
	int 21h
	jc FileCloseBadHandle
CloseFileEnd:
	pop bx
	pop ax
	ret
	FileCloseBadHandle:
		SHOW errClosingBadHandle
		jmp CloseFileEnd
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
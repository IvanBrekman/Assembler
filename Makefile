DIR  = NASM
FILE = printf
EXPN = asm

OUT  = cat.purr

cr:
	clear
	
	nasm $(DIR)/$(FILE).$(EXPN) -f elf64  -l $(DIR)/$(FILE).lst
	ld   $(DIR)/$(FILE).o       -o $(OUT) -s

	./$(OUT)

c:
	nasm $(DIR)/$(FILE).$(EXPN) -f elf64  -l $(DIR)/$(FILE).lst 
	ld   $(DIR)/$(FILE).o       -o $(OUT) -s

r:
	./$(OUT)


dbg:
	edb --run $(OUT)

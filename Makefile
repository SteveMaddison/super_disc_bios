all: bios.sfc

clean:
	rm -f main.o bios.sfc bios.sym

main.o: main.asm 
	wla-65816 main.asm 

bios.sfc: main.o
	wlalink -S linkfile bios.sfc

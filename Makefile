testprog: output.o asmlib.o
	ld -o $@ $^

output.o: src/tests/testsrc
	cargo r build $<

asmlib.o: asmlib.s
	nasm -f elf64 -o $@ $<

run: testprog
	./testprog

clean:
	rm output.o asmlib.o testprog

.PHONY: run clean

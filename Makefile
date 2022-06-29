testprog: output.o lowlib.o
	ld -o $@ $^

output.o: testsrc
	cargo r build $<

lowlib.o: lowlib.s
	nasm -f elf64 -o $@ $<

run: testprog
	./testprog

clean:
	rm output.o lowlib.o testprog

.PHONY: run clean

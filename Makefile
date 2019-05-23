all:
	$(CC) -o meta meta.c
recomp: meta meta-c.txt
	./meta meta-c.txt meta.c && $(CC) -o meta meta.c && ./meta meta-c.txt meta.c
forth_bootstrap:  meta meta-forth.txt
	./meta meta-forth.txt forth.c && $(CC) -o forth forth.c && ./forth meta-forth.txt meta.fs
forth_recomp: meta meta-forth.txt
	gforth meta.fs meta-forth.txt meta.fs

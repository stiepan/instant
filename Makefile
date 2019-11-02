all:
	ghc --make -isrc/:src/grammar src/InscJvm.hs -o insc_jvm

clean:
	-find . -type f -name '*.o' -delete
	-find . -type f -name '*.hi' -delete

distclean: clean
	-rm -f insc_jvm
	


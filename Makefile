
make: stuff stufff

bench: 
	ghc --make -O2 bench
	time ./bench
	make clean

clean: 
	rm *.o *.hi example bench bench2

stufff:
	echo 1


stuff:
	ghc --make example




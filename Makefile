test:
	cd tests && make test
	cd testfs && make test-web

doc: build-sadoxygen-xml
	doxygensake html

build-doxygasdfd-party && make
sdf
install-testing-dependencies:
	cd third-party && make install-testing-dependencies

clean:
	rm -rf build/*
	cd tests && make clean
clean-dep:
	cd third-party && make clean
asdf
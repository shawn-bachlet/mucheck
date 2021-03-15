build:
	cabal v1-install --only-dependencies --enable-tests
	cabal v1-configure --enable-tests
	cabal v1-build

hlint:
	(cd support; ~/.cabal/bin/hlint `find ../src -name \*.hs`)

clean-sandbox:
	- cabal v1-sandbox hc-pkg unregister DMuCheck
	- cabal v1-sandbox hc-pkg unregister MuCheck-QuickCheck
	- cabal v1-sandbox hc-pkg unregister MuCheck-SmallCheck
	- cabal v1-sandbox hc-pkg unregister MuCheck-HUnit
	- cabal v1-sandbox hc-pkg unregister MuCheck-Hspec
	- cabal v1-sandbox hc-pkg unregister MuCheck

sandbox:
	mkdir -p ../mucheck-sandbox
	cabal v1-sandbox init --sandbox ../mucheck-sandbox

install:
	cabal v1-install

prepare:
	cabal v1-haddock
	cabal v1-check
	cabal v1-test
	cabal v1-sdist

clean:
	- rm Examples/*.hi Examples/*.o
	- rm *.log

.PHONY: test
test:
	cabal v1-test

# :set -package QuickCheck
# :m +Test.QuickCheck
testrepl:
	cabal v1-repl --ghc-option='-package QuickCheck-2.6'

hpcex:
	- rm Examples/*.hi Examples/*.o *.tix tests
	cabal v1-build sample-test
	cabal v1-build mucheck
	./dist/build/sample-test/sample-test
	./dist/build/mucheck/mucheck -tix sample-test.tix Examples/AssertCheckTest.hs

#hpcex:
#	- rm dist-newstyle *.tix tests
#	cabal build
#	./dist-newstyle/build/x86_64-osx/ghc-8.10.3/MuCheck-0.4.0.0/x/sample-test/build/sample-test/sample-test
#	./dist-newstyle/build/x86_64-osx/ghc-8.10.3/MuCheck-0.4.0.0/x/mucheck/build/mucheck/mucheck -tix sample-test.tix Examples/AssertCheckTest.hs
#
install-all: install
	for i in ../mucheck-smallcheck ../mucheck-quickcheck ../mucheck-hunit ../mucheck-hspec; do (cd $$i; make install ); done

# ----------------------------------------------------------------- [ Makefile ]
# Module    : Makefile
# Copyright : (c) Jan de Muijnck-Hughes
# License   : see LICENSE
# ---------------------------------------------------------------------- [ EOH ]

IDRIS   := idris
LANGLIB := sif
IOLIB   := sifio
PRELUDE := sifprelude
EXE     := sifexe
MONO    := sifmono

.PHONY: doc clobber check clean io lang linecount testLang testIO tests prelude all monolithic perfbuild

lang:
	${IDRIS} --build ${LANGLIB}.ipkg
	${IDRIS} --install ${LANGLIB}.ipkg

prelude:
	${IDRIS} --build ${PRELUDE}.ipkg
	${IDRIS} --install ${PRELUDE}.ipkg

io:
	${IDRIS} --build ${IOLIB}.ipkg
	${IDRIS} --install ${IOLIB}.ipkg

exe:
	${IDRIS} --build ${EXE}.ipkg

all: lang prelude io exe

monolithic:
	${IDRIS} --build ${MONO}.ipkg

perfbuild: clobber
	/usr/bin/time -p ${IDRIS} --checkpkg ${MONO}.ipkg
	/usr/bin/time -p ${IDRIS} --build ${MONO}.ipkg
	${IDRIS} --clean ${MONO}.ipkg
	/usr/bin/time -p ${IDRIS} --build ${MONO}.ipkg

clean:
	${IDRIS} --clean ${LANGLIB}.ipkg
	${IDRIS} --clean ${IOLIB}.ipkg
	${IDRIS} --clean ${EXE}.ipkg
	find . -name "*~" -delete

check: clobber
	${IDRIS} --checkpkg ${LANGLIB}.ipkg
	${IDRIS} --checkpkg ${IOLIB}.ipkg
	${IDRIS} --checkpkg ${EXE}.ipkg
	${IDRIS} --checkpkg ${MONO}.ipkg


clobber : clean
	find . -name "*.ibc" -delete

doc:
	${IDRIS} --mkdoc ${LANGLIB}.ipkg
	${IDRIS} --mkdoc ${IOLIB}.ipkg
	${IDRIS} --mkdoc ${EXE}.ipkg

# testLang: lang

# ${IDRIS} --testpkg ${LANGLIB}.ipkg

testIO: lang
	${IDRIS} --testpkg ${IOLIB}.ipkg

tests: testIO


linecount:
	find . -name "*.idr" | grep -v Lib |xargs wc -l -

# ---------------------------------------------------------------------- [ EOF ]


# set name of package and any extra contents for .tar file
PACKAGE = clipmon
CONTENTS := beepbeep.wav

VERSION != grep ";; Version:" ${PACKAGE}.el | grep -E -o [0-9]+
# DESCRIPTION != grep ";;; ${PACKAGE}.el --- " ${PACKAGE}.el | grep -E -o [0-9]+
# HOMEPAGE !=

EMACS = emacs
CASK = cask


help:
	@echo ""
	@echo "make test     run unit tests in <package>-test.el file"
	@echo "make all      clean, compile, pkg, readme, tar"
	@echo "make compile  compile .el files to .elc"
	@echo "make pkg      make <package>-pkg.el file for multi-file packages"
	@echo "make readme   make README.md from <package>.el commentary section"
	@echo "make tar      make <package>-<version>.tar package file for package.el"
	@echo "make clean    delete readme, -pkg, .elc, .tar"


test: compile
	${EMACS} -Q -batch -L . -l ${PACKAGE}-test.el -f ert-run-tests-batch


all: clean compile pkg readme tar

compile:
	${EMACS} -Q -batch -L . --eval="(byte-compile-file \"${PACKAGE}.el\")"

pkg:
	${CASK} pkg-file
	cat ${PACKAGE}-pkg.el

# 	echo "{define-package \"${PACKAGE}\" \"${VERSION}\" \"${DESCRIPTION}\"}" > ${PACKAGE}-pkg.el

readme:
	${EMACS} --script make-readme-markdown.el <${PACKAGE}.el >README.md
	head -5 README.md
	tail -5 README.md
#	cat README.md


PACKAGE_DIR := ${PACKAGE}-${VERSION}
PACKAGE_TAR := ${PACKAGE}-${VERSION}.tar

tar:
	mkdir ${PACKAGE_DIR}
	cp ${PACKAGE}.el ${PACKAGE}-pkg.el ${PACKAGE_DIR}
	cp ${CONTENTS} ${PACKAGE_DIR}
	tar -cf ${PACKAGE_TAR} ${PACKAGE_DIR}
	tar -tvf ${PACKAGE_TAR}

clean:
	rm -f *.elc
	rm -f README.md
	rm -f ${PACKAGE}-pkg.el
	rm -f ${PACKAGE_TAR}
	rm -rdf ${PACKAGE_DIR}



.PHONY: all help test readme pkg compile tar clean

# end


# set name of package and any extra contents for .tar file
PACKAGE = clipmon
CONTENTS := beepbeep.wav

EMACS = emacs
CASK = cask


all: help

help:
	@echo "make test     run unit tests in <package>-test.el file"
	@echo "make compile  compile .el files to .elc"
	@echo "make pkg      make <package>-pkg.el file for multi-file packages"
	@echo "make readme   make README.md from <package>.el commentary section"
	@echo "make tar      make <package>-<version>.tar package file for package.el"
	@echo "make clean    delete readme, -pkg, .elc, .tar"


VERSION != grep ";; Version:" ${PACKAGE}.el | grep -E -o [0-9]+
PACKAGE_DIR := ${PACKAGE}-${VERSION}
PACKAGE_TAR := ${PACKAGE}-${VERSION}.tar

test:
	${EMACS} -batch -L . -l ${PACKAGE}-test.el -f ert-run-tests-batch

compile:
	${EMACS} -batch -L . --eval="(byte-compile-file \"${PACKAGE}.el\")"

compile1:
	${EMACS} -Q -batch -f batch-byte-compile f.el

pkg:
	${CASK} pkg-file

# pkg:
# 	echo "{define-package \"clipmon\" \"${VERSION}\" \"Clipboard monitor - automatically pastes clipboard changes.\"}" > clipmon-pkg.el
# clipmon-pkg-template.el:
# sed -re "s/VERSION/${VERSION}/" $@/clipmon-pkg-template.el > $@/clipmon-pkg.el
# 	echo {define-package "clipmon" "VERSION" "Clipboard monitor - automatically pastes clipboard changes."} > clipmon-pkg-template.el


readme:
	${EMACS} --script make-readme-markdown.el <${PACKAGE}.el >README.md

tar:
	mkdir ${PACKAGE_DIR}
	cp ${PACKAGE}.el ${PACKAGE}-pkg.el ${PACKAGE_DIR}
	cp ${CONTENTS} ${PACKAGE_DIR}
	tar -cf ${PACKAGE_TAR} ${PACKAGE_DIR}

clean:
	rm -f *.elc
	rm -f README.md
	rm -f ${PACKAGE}-pkg.el
	rm -f ${PACKAGE_TAR}
	rm -rd ${PACKAGE_DIR}



.PHONY: all help test readme pkg compile tar clean

# end

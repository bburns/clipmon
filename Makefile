# --------------------------------------------------------------------------------
# makefile for emacs packages
# --------------------------------------------------------------------------------


# set name of package
PACKAGE = clipmon

# set any extra contents for the .tar file here
CONTENTS := clipmon.wav


# --------------------------------------------------------------------------------

# programs
EMACS = emacs
CASK = cask

# files
SOURCE = ${PACKAGE}.el
PKG = ${PACKAGE}-pkg.el
TEST = ${PACKAGE}-test.el
PACKAGE_DIR := ${PACKAGE}-${VERSION}
PACKAGE_TAR := ${PACKAGE}-${VERSION}.tar

# parse package metadata
# currently handles only one keyword, and no dependencies
# eg need :keywords '("speed" "convenience"))
#. better to call an elisp fn to parse this stuff and make -pkg file
# note: grep -P for perl regexp, -o to just output match
# ?<= is the look-behind operator - match is not included in output
DESCRIPTION  != grep ";;; ${SOURCE} --- " ${SOURCE} | grep -Po "(?<= --- ).+"
VERSION      != grep ";; Version:"        ${SOURCE} | grep -Po [0-9]+
HOMEPAGE     != grep ";; URL:"            ${SOURCE} | grep -Po "(?<=;; URL: ).+"
KEYWORDS     != grep ";; Keywords:"       ${SOURCE} | grep -Po "(?<=;; Keywords: ).+"
KEYWORDS := "\"${KEYWORDS}\""
DEPENDENCIES = "nil"


# --------------------------------------------------------------------------------


help:
	@echo ""
	@echo "make info       show package info extracted from <package>.el"
	@echo "make test       run unit tests in <package>-test.el file"
	@echo "make all        clean, compile, pkg, readme, tar"
	@echo "make compile    compile .el files to .elc"
	@echo "make pkg        make <package>-pkg.el file for multi-file packages"
	@echo "make readme     make README.md from <package>.el commentary section"
	@echo "make tar        make <package>-<version>.tar package file"
	@echo "make clean      delete readme, -pkg, .elc, .tar"
	@echo "make clean-elc  delete .elc"
	@echo ""


info:
	@echo "Package:       ${PACKAGE}"
	@echo "Description:   ${DESCRIPTION}"
	@echo "Homepage:      ${HOMEPAGE}"
	@echo "Version:       ${VERSION}"
	@echo "Keywords:      ${KEYWORDS}"
	@echo "Dependencies:  ${DEPENDENCIES}"
	@echo ""


test: compile
	${EMACS} -Q -batch -L . -l ${TEST} -f ert-run-tests-batch


all: info clean compile test pkg readme tar


# need -L . so tests can (require 'clipmon)
compile:
	${EMACS} -Q -batch -L . --eval="(byte-compile-file \"${SOURCE}\")"

# compile1:
# 	${EMACS} -Q -batch -L . -f batch-byte-compile *.el


pkg:
	@echo "(define-package \"${PACKAGE}\" \"${VERSION}\"" > ${PKG}
	@echo "  \"${DESCRIPTION}\""        >> ${PKG}
	@echo "  ${DEPENDENCIES}"           >> ${PKG}
	@echo "  :url \"${HOMEPAGE}\""      >> ${PKG}
	@echo "  :keywords '(${KEYWORDS}))" >> ${PKG}
	cat ${PKG}

# cask is asynchronous
# pkg0:
# 	@echo "Running cask - hit Enter when done..."
# 	${CASK} pkg-file
# 	read
# 	cat ${PKG}


readme:
	rm -f README.md
	${EMACS} --script make-readme.el <${SOURCE} >README.md
	attrib +r README.md
	head -5 README.md
	tail -9 README.md


tar:
	rm -rdf ${PACKAGE_DIR}
	mkdir ${PACKAGE_DIR}
	cp ${SOURCE} ${PACKAGE_DIR}
	cp ${PKG} ${PACKAGE_DIR}
	cp ${CONTENTS} ${PACKAGE_DIR}
	tar -cf ${PACKAGE_TAR} ${PACKAGE_DIR}
	tar -tvf ${PACKAGE_TAR}


clean:
	rm -f *.elc
	rm -f README.md
	rm -f ${PKG}
	rm -f ${PACKAGE_TAR}
	rm -rdf ${PACKAGE_DIR}


clean-elc:
	rm -f *.elc


.PHONY: help info test all compile pkg readme tar clean clean-elc


# end

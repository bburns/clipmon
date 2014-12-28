
# clipmon makefile

# uses modified make-readme-markdown.el to convert elisp comments to readme file
# see https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el


help:
	@echo "make readme   make README.md from .el commentary section"
	@echo "make test     run unit tests in -test.el file"
	@echo "make compile  compile .el files to .elc"
	@echo "make clean    delete readme, .elc files"


readme: README.md
README.md: clipmon.el
	emacs --script make-readme-markdown.el <clipmon.el >$@ 2>/dev/null

test:
	emacs -batch -L . -l clipmon-test.el -f ert-run-tests-batch

compile:
	emacs -batch -L . --eval="(byte-compile-file \"clipmon.el\")"

clean:
	rm -f README.md
	rm -f *.elc







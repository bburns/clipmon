

# make readme from elisp commentary
# $ wget --no-check-certificate https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el
README.md: clipmon.el
	emacs --script make-readme-markdown.el <clipmon.el >$@ 2>/dev/null

# run ert tests
test:
	emacs -batch -L . -l clipmon-test.el -f ert-run-tests-batch



# from make-readme-markdown:

# README.md: make-readme-markdown.el clipmon.el
#	emacs --script $< <clipmon.el >$@ 2>/dev/null

# make-readme-markdown.el:
#	wget --no-check-certificate https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el

# .INTERMEDIATE: make-readme-markdown.el





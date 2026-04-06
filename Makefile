.PHONY: test lint clean

test:
	emacs -batch -L . -f batch-byte-compile org-mindmap-parser.el org-mindmap.el
	emacs -batch -L . -l tests/test-parsing.el
	emacs -batch -L . -l tests/test-editing.el -f ert-run-tests-batch-and-exit

lint:
	emacs -batch -f package-initialize -L . --eval "(require 'package-lint)" -f package-lint-batch-and-exit org-mindmap.el org-mindmap-parser.el
	emacs -batch -L . --eval "(require 'checkdoc)" --eval "(checkdoc-file \"org-mindmap.el\")" --eval "(checkdoc-file \"org-mindmap-parser.el\")"

clean:
	rm -f *.elc *~

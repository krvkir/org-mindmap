test:
	emacs -batch -L . -f batch-byte-compile org-mindmap-parser.el org-mindmap.el \
		&& emacs -batch -L . -l run_tests.el -f ert-run-tests-batch-and-exit \
		&& emacs -batch -L . -l test-editing.el -f ert-run-tests-batch-and-exit

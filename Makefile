SCHEME = chezscheme --libdirs lib/:srfi-213/lib/:srfi-237/lib/:tests/lib/ --program

check:
	$(SCHEME) tests.sps

.PHONY: check

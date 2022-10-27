SCHEME = chezscheme --libdirs lib/:srfi-213/lib/ --program

check:
	$(SCHEME) tests.sps

.PHONY: check

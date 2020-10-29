bin/loading: bin/loading.hs scripts/build.sh
	scripts/build.sh

.PHONY: run
run: bin/loading
	bin/loading

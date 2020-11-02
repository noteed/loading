bin/loading: bin/loading.hs scripts/build.sh
	scripts/build.sh

screenshot.png: bin/loading
	bin/loading headless

.PHONY: run
run: bin/loading
	bin/loading

.PHONY: view
view: screenshot.png
	feh --borderless --force-aliasing -g 1920x1200+320+120 --zoom fill $<

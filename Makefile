bin/loading: bin/loading.hs scripts/build.sh
	scripts/build.sh

screenshot.png: bin/loading
	bin/loading screenshot

.PHONY: run
run: bin/loading
	bin/loading run

.PHONY: view
view: screenshot.png
	feh --borderless --force-aliasing -g 1920x1200+320+120 --zoom fill $<

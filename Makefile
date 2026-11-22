.PHONY: all update build optim serve clean

EXE_NAME := guess-blue-prince-room-web

all: update build optim

update:
	wasm32-wasi-cabal update

build:
	wasm32-wasi-cabal build exe:$(EXE_NAME)
	rm -rf public
	cp -r web/static public
	@if [ -n "$$MIELO_CSS_DIR" ] && [ -f "$$MIELO_CSS_DIR/mielo.min.css" ]; then \
		cp -v "$$MIELO_CSS_DIR/mielo.min.css" public/mielo.min.css; \
	fi
	$(eval MY_WASM := $(shell wasm32-wasi-cabal list-bin exe:$(EXE_NAME) | tail -n 1))
	"$$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" --input "$(MY_WASM)" --output public/ghc_wasm_jsffi.js
	cp -v "$(MY_WASM)" public/app.wasm

optim:
	wasm-opt -all --disable-compact-imports -O2 public/app.wasm -o public/app.wasm
	wasm-tools strip -o public/app.wasm public/app.wasm

serve:
	python3 -m http.server 8080 -d public

clean:
	rm -rf dist-newstyle public

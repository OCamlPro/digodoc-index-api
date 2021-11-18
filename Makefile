PROJECT_NAME:=digodoc-index-api
API_HOST:=http://localhost:11001
CONTACT_EMAIL:=mohamed.hernouf@ocamlpro.com
VERSION:=1.0

.EXPORT_ALL_VARIABLES:
PGDATABASE:=digodoc
API_PORT:=49002
DIGODOC_DIR:=<digodoc_docs_dir>

all: build api-server

db-updater:
	@dune build src/db/db-update

db-update: db-updater
	@_build/default/src/db/db-update/db_updater.exe --allow-downgrade --database $(PGDATABASE)

db-downgrade: db-updater
	$(eval DBVERSION := $(shell psql $(PGDATABASE) -c "select value from ezpg_info where name='version'" -t -A))
	_build/default/src/db/db-update/db_updater.exe --allow-downgrade --database $(DATABASE) --target `expr $(DBVERSION) - 1`

db-version:
	psql $(PGDATABASE) -c "select value from ezpg_info where name='version'" -t -A

docs: build
	dune build @doc
	dune build @doc-private
	mkdir -p docs
	rm -rf docs/*  
	cp -r _build/default/_doc/_html/* docs/.

build: db-update
	dune build --profile release

api-server: _build/default/src/api/api_server.exe
	@mkdir -p bin
	@cp -f _build/default/src/api/api_server.exe bin/api-server

clean:
	@dune clean
	rm -rf docs api bin logs

install:
	@dune install

build-deps:
	@opam install --deps-only .

config/api_config.json:
	@mkdir -p config
	@echo "{\"port\": $(API_PORT)}" > config/api_config.json

depext:
	opam install opam-depext -y
	opam depext geoip -y

init: depext build-deps config

git-init:
	rm -rf .git
	git init

openapi: _build/default/src/api/openapi.exe
	@mkdir -p api
	@_build/default/src/api/openapi.exe --version $(VERSION) --title "$(PROJECT_NAME) API" --contact "$(CONTACT_EMAIL)" --servers "api" $(API_HOST) -o api/openapi.json

view-api: openapi
	xdg-open 'http://localhost:28882' & redoc-cli serve -p 28882 -w api/openapi.json

view-docs: docs
	xdg-open 'http://localhost:50002' & php -S localhost:50002 -t docs
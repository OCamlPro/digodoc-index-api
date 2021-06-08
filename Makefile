PROJECT_NAME:=digodoc-indexer
DATABASE:=digodoc
API_HOST:=http://localhost:49002
API_PORT:=49002
RLS_DIR:=www
CONTACT_EMAIL:=
VERSION:=1.0
DBVERSION=$(shell psql $(DATABASE) -c "select value from ezpg_info where name='version'" -t -A)

-include Makefile.config

.EXPORT_ALL_VARIABLES:

PGDATABASE=$(DATABASE)

all: build api-server openapi

db-updater:
	@dune build src/db/db-update

db-update: db-updater
	@_build/default/src/db/db-update/db_updater.exe --allow-downgrade --database $(PGDATABASE)

db-downgrade: db-updater
	_build/default/src/db/db-update/db_updater.exe --allow-downgrade --database $(DATABASE) --target `expr $(DBVERSION) - 1`

build: db-update
	dune build --profile release

api-server: _build/default/src/api/api_server.exe
	@mkdir -p bin
	@cp -f _build/default/src/api/api_server.exe bin/api-server

clean:
	@dune clean

install:
	@dune install

build-deps:
	@opam install --deps-only .

config/info.json config/api_config.json:
	@mkdir -p config
	@echo "{\"apis\": [\"$(API_HOST)\"]}" > config/info.json
	@echo "{\"port\": $(API_PORT)}" > config/api_config.json

init: build-deps config

git-init:
	rm -rf .git
	git init

openapi: _build/default/src/api/openapi.exe
	@_build/default/src/api/openapi.exe --version $(VERSION) --title "$(PROJECT_NAME) API" --contact "$(CONTACT_EMAIL)" --servers "api" $(API_HOST) -o www/openapi.json

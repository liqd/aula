SHELL=/bin/bash
EXEC=`test -d .stack-work/ && echo "stack exec --" || echo "cabal exec --"`
HLINT=$(EXEC) hlint
AULA_SOURCES=-isrc -itests -idist/build/autogen
FULL_SOURCES=$(AULA_SOURCES) -i$(THENTOS_ROOT_PATH)/src/ -i$(THENTOS_ROOT_PATH)/../thentos-tests/src/ -i$(THENTOS_ROOT_PATH)/../thentos-tests/tests/
AULA_IMAGE=quay.io/liqd/aula

.phony:

# use sensei if you only want to hack aula; use sensei-full if you
# want to hack both aula and thentos.
#
# [fisx] the unregister rules are a hack: i experienced problems with
# getting changes to source files noticed when any of the packages i
# was sensei-ing were installed in the sandbox.  since i am
# unregistering them as a precondition of the sensei rules, that
# problem went away.  (more data on this or hypotheses on the source
# of the problem are always welcome.)

%.unregister:
	-$(EXEC) ghc-pkg unregister $*

unregister-full:
	make thentos-adhocracy.unregister thentos-tests.unregister thentos-core.unregister aula.unregister

# only aware of aula sources
sensei: .phony aula.unregister
	$(EXEC) sensei $(AULA_SOURCES) tests/Spec.hs $(SENSEI_DEFAULT_ARGS) $(SENSEI_ARGS)

stories: .phony aula.unregister
	$(EXEC) sensei $(AULA_SOURCES) tests/AulaTests/StoriesSpec.hs $(SENSEI_DEFAULT_ARGS) $(SENSEI_ARGS)

# aware of aula and thentos sources
sensei-full: .phony unregister-full
	$(EXEC) sensei $(FULL_SOURCES) -optP-DDEVELOPMENT ./tests/Spec.hs $(SENSEI_DEFAULT_ARGS) $(SENSEI_ARGS)

seito: .phony
	sleep 0.2 && seito

aula-server: .phony
	$(EXEC) runhaskell $(FULL_SOURCES) ./exec/Aula.hs

click-dummies-recreate: .phony
	@echo "*** this target is deprecated!"
	@echo "*** you need pre-created sample sources to use click-dummies-refresh now."
	@echo "*** see module docs in src/RenderHtml.hs"

click-dummies-refresh: .phony aula.unregister
	$(EXEC) sensei $(FULL_SOURCES) ./exec/RenderHtml.hs

test-repl:
	$(EXEC) ghci $(FULL_SOURCES)

hlint:
	$(HLINT) --version
	find src exec tests -name '*.hs' | xargs $(HLINT)

test-everything:
	make hlint
	cabal install --enable-test --ghc-options="-Werror -Wall -O0"
	cabal test

ghci-no-type-errors:
	$(EXEC) ghci $(FULL_SOURCES) -fdefer-type-errors

seito-docker-hack:
	pwd > pwd.log
	docker exec -it `docker ps -q --filter="ancestor=$(AULA_IMAGE)"` /liqd/aula/docker/make-seito.sh

wc:
	find src tests -name '*.hs' | xargs wc

content:
	curl -XPOST http://localhost:8080/api/manage-state/create-init
	curl -XPOST http://localhost:8080/api/manage-state/create-demo

tags: .phony
	hasktags -b src/ tests/ exec/ dist/build/autogen/

grep.%:
	git grep -Hni $*

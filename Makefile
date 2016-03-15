SHELL=/bin/bash
EXEC=`test -d .stack-work/ && echo "stack exec --" || echo "cabal exec --"`
HLINT=$(EXEC) hlint
FULL_SOURCES=-isrc -itests -i$(THENTOS_ROOT_PATH)/src/ -i$(THENTOS_ROOT_PATH)/../thentos-tests/src/ -i$(THENTOS_ROOT_PATH)/../thentos-tests/tests/
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
	$(EXEC) sensei -isrc -itests tests/Spec.hs $(SENSEI_ARGS)

# aware of aula and thentos sources
sensei-full: .phony unregister-full
	$(EXEC) sensei $(FULL_SOURCES) -optP-DDEVELOPMENT ./tests/Spec.hs $(SENSEI_ARGS)

seito: .phony
	sleep 0.2 && seito

aula-server: .phony
	$(EXEC) runhaskell $(FULL_SOURCES) ./exec/Aula.hs

click-dummies-recreate: .phony
	$(EXEC) runhaskell $(FULL_SOURCES) ./exec/RenderHtml.hs

click-dummies-refresh: .phony aula.unregister
	$(EXEC) sensei $(FULL_SOURCES) ./exec/RenderHtml.hs

test-repl:
	$(EXEC) ghci $(FULL_SOURCES)

hlint:
	$(HLINT) --version
	find src exec tests -name '*.hs' | xargs $(HLINT)

test-everything:
	perl -i -pe 's/ghc-options: -Wall/ghc-options: -Wall -Werror/' package.yaml
	hpack
	cabal install --enable-test
	make hlint
	make click-dummies-recreate
	git checkout -- package.yaml
	hpack

ghci-no-type-errors:
	$(EXEC) ghci $(FULL_SOURCES) -fdefer-type-errors

seito-docker-hack:
	pwd > pwd.log
	docker exec -it `docker ps -q --filter="ancestor=$(AULA_IMAGE)"` /liqd/aula/docker/make-seito.sh

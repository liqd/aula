HLINT=hlint
FULL_SOURCES=-isrc -itests -i$(THENTOS_ROOT_PATH)/src/ -i$(THENTOS_ROOT_PATH)/../thentos-tests/src/ -i$(THENTOS_ROOT_PATH)/../thentos-tests/tests/

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
	-cabal exec -- ghc-pkg unregister $*

unregister-full:
	make thentos-adhocracy.unregister thentos-tests.unregister thentos-core.unregister aula.unregister

# only aware of aula sources
sensei: .phony aula.unregister
	cabal exec -- sensei -isrc -itests tests/Bla.hs $(SENSEI_ARGS)

# aware of aula and thentos sources
sensei-full: .phony unregister-full
	cabal exec -- sensei $(FULL_SOURCES) -optP-DDEVELOPMENT ./tests/Spec.hs $(SENSEI_ARGS)

seito: .phony
	sleep 0.2 && seito

aula-server: .phony
	cabal exec -- runhaskell $(FULL_SOURCES) ./exec/Aula.hs

click-dummies-recreate: .phony
	cabal exec -- runhaskell $(FULL_SOURCES) ./exec/RenderHtml.hs

click-dummies-refresh: .phony aula.unregister
	cabal exec -- sensei $(FULL_SOURCES) ./exec/RenderHtml.hs

test-repl:
	cabal exec -- ghci $(FULL_SOURCES)

hlint:
	$(HLINT) --version
	find src exec tests -name '*.hs' | xargs $(HLINT)

ghci-no-type-errors:
	cabal exec -- ghci $(FULL_SOURCES) -fdefer-type-errors

.phony:

# only aware of aula sources
sensei: .phony
	cabal exec -- sensei -isrc -itests tests/Spec.hs

THENTOS_SOURCES=-isrc -itests -i$(THENTOS_ROOT_PATH)/thentos-core/src/ -i$(THENTOS_ROOT_PATH)/thentos-tests/src/ -i$(THENTOS_ROOT_PATH)/thentos-tests/tests/

# aware of aula and thentos sources
sensei-full: .phony
	cabal exec -- sensei $(FULL_SOURCES) -optP-DDEVELOPMENT ./tests/Spec.hs $(SENSEI_ARGS)

repl-full: .phony prepare-repl
	cabal exec -- ghci $(FULL_SOURCES) -optP-DDEVELOPMENT ./exec/Aula.hs

seito: .phony
	sleep 0.2 && seito

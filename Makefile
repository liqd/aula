HLINT=hlint
FULL_SOURCES=-isrc -itests -i$(THENTOS_ROOT_PATH)/thentos-core/src/ -i$(THENTOS_ROOT_PATH)/thentos-tests/src/ -i$(THENTOS_ROOT_PATH)/thentos-tests/tests/

.phony:

# only aware of aula sources
sensei: .phony
	cabal exec -- sensei -isrc -itests tests/Spec.hs $(SENSEI_ARGS)

# aware of aula and thentos sources
sensei-full: .phony
	cabal exec -- sensei $(FULL_SOURCES) -optP-DDEVELOPMENT ./tests/Spec.hs $(SENSEI_ARGS)

seito: .phony
	sleep 0.2 && seito

aula-server: .phony
	cabal exec -- ghci $(FULL_SOURCES) ./exec/Aula.hs

aula-server-run: .phony
	cabal run -- aula-server

click-dummies-recreate: .phony
	cabal exec -- runhaskell $(FULL_SOURCES) ./exec/RenderHtml.hs --recreate

click-dummies-refresh: .phony
	cabal exec -- runhaskell $(FULL_SOURCES) ./exec/RenderHtml.hs --refresh

test-repl:
	cabal exec -- ghci $(FULL_SOURCES)

hlint:
	$(HLINT) --version
	find src exec -name '*.hs' | xargs $(HLINT)

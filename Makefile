.phony:

sensei: .phony
	cabal exec -- sensei -isrc src/Types.hs

seito: .phony
	sleep 0.2 && seito

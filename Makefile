.phony:

sensei: .phony
	cabal exec -- sensei -isrc exec/RenderHtml.hs

seito: .phony
	sleep 0.2 && seito

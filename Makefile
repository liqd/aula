SHELL=/bin/bash
EXEC=`test -d .stack-work/ && echo "stack exec --allow-different-user --" || echo "cabal exec --"`
HLINT=$(EXEC) hlint
AULA_SOURCES=-isrc -itests -iexec -idist/build/autogen
AULA_IMAGE=quay.io/liqd/aula:aula-docker-0.3

.phony:

# use sensei if you only want to hack aula.
#
# [fisx] the unregister rules are a hack: i experienced problems with
# getting changes to source files noticed when any of the packages i
# was sensei-ing were installed in the sandbox.  since i am
# unregistering them as a precondition of the sensei rules, that
# problem went away.  (more data on this or hypotheses on the source
# of the problem are always welcome.)

%.unregister:
	-$(EXEC) ghc-pkg unregister $*

sensei: .phony aula.unregister
	$(EXEC) sensei -j5 $(AULA_SOURCES) tests/Spec.hs $(SENSEI_DEFAULT_ARGS) $(SENSEI_ARGS) --skip @Large --skip @Selenium

selenium: .phony aula.unregister
	$(EXEC) sensei -j5 $(AULA_SOURCES) tests/Spec.hs $(SENSEI_DEFAULT_ARGS) $(SENSEI_ARGS) --match @Selenium

sensei-large: .phony aula.unregister
	$(EXEC) sensei -j5 $(AULA_SOURCES) -optP-DDEVELOPMENT ./tests/Spec.hs $(SENSEI_DEFAULT_ARGS) $(SENSEI_ARGS)

stories: .phony aula.unregister
	$(EXEC) sensei -j5 $(AULA_SOURCES) tests/AulaTests/StoriesSpec.hs $(SENSEI_DEFAULT_ARGS) $(SENSEI_ARGS) --skip @Large

seito: .phony
	sleep 0.2 && seito

seito-docker: .phony
	pwd > pwd.log
	docker exec -it `docker ps -q --filter="ancestor=$(AULA_IMAGE)"` /liqd/aula/docker/make-seito.sh

aula-server: .phony
	$(EXEC) runhaskell -j5 $(AULA_SOURCES) ./exec/Aula.hs

click-dummies-recreate: .phony
	@echo "*** this target is deprecated!"
	@echo "*** you need pre-created sample sources to use click-dummies-refresh now."
	@echo "*** see module docs in src/RenderHtml.hs"

click-dummies-refresh: .phony aula.unregister
	$(EXEC) sensei $(AULA_SOURCES) ./exec/RenderHtml.hs

test-repl:
	$(EXEC) ghci $(AULA_SOURCES)

hlint:
	$(HLINT) --version
	find src exec tests -name '*.hs' | xargs $(HLINT)

test-everything:
	make hlint
	cabal install --enable-test --ghc-options="-Werror -Wall -O0"
	cabal test

ghci-no-type-errors:
	$(EXEC) ghci $(AULA_SOURCES) -fdefer-type-errors

wc:
	find src tests -name '*.hs' | xargs wc

content-login: .phony
	rm -f cookie-jar
	[ "$(AULA_MK_CONTENT_URL)" != "" ] || ( echo "set with e.g. 'export AULA_MK_CONTENT_URL=http://localhost:8080'"; false )
	curl -c cookie-jar -F /login.user=admin -F /login.pass=pssst $(AULA_MK_CONTENT_URL)/login

content:
	curl -XPOST $(AULA_MK_CONTENT_URL)/api/manage-state/create-init
	make content-login
	curl -b cookie-jar -XPOST $(AULA_MK_CONTENT_URL)/api/manage-state/create-demo
	curl -b cookie-jar -XPOST $(AULA_MK_CONTENT_URL)/api/manage-state/create-votes
	rm -f cookie-jar

content-deleg:
	make content-login
	curl -b cookie-jar -XPOST $(AULA_MK_CONTENT_URL)/api/manage-state/create-delegations
	rm -f cookie-jar

tags: .phony
	hasktags -b src/ tests/ exec/ dist/build/autogen/

grepi.%:
	git grep -Hni $* src tests exec

grep.%:
	git grep -Hn $* src tests exec

docker-hpc:
	./.travis/docker-build.sh 1000
	cp -R /liqd/aula/.stack-work/install/x86_64-linux/lts-3.20/7.10.2/hpc .

team-avatars: .phony
	-rm -r team-avatars
	mkdir team-avatars
	curl -o team-avatars/andorp https://avatars1.githubusercontent.com/u/3465327?v=3&s=400
	curl -o team-avatars/fisx https://avatars2.githubusercontent.com/u/10210727?v=3&s=400
	curl -o team-avatars/np https://avatars3.githubusercontent.com/u/5548?v=3&s=400
	curl -o team-avatars/rittermo https://avatars3.githubusercontent.com/u/15341015?v=3&s=400
	curl -o team-avatars/localgrr https://avatars1.githubusercontent.com/u/701632?v=3&s=400
	curl -o team-avatars/mikolaj https://avatars1.githubusercontent.com/u/281893?v=3&s=400
	$(EXEC) runhaskell -j5 $(AULA_SOURCES) ./exec/ResizeAvatar.hs 300 100 64 32 -- ./team-avatars/andorp ./team-avatars/fisx ./team-avatars/localgrr ./team-avatars/mikolaj ./team-avatars/np ./team-avatars/rittermo
	@echo "Open now your web browser on: file://$$PWD/avatars.html"

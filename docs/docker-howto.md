# thoughts on how to use docker

On the one hand, docker makes it easy to handle system state; on the
other, it creates a lot of traffic volumes, and debugging cycles of
half an hour or more can lead to long outages if something vital
breaks.

This document collects the lessons learned with using docker, and is
our shot at docker best practices.


##

- docker failure process.
  - revert?  (how?)
  - ..
  - post mortem.  what was the problem.
    lessons learned from docker:
       - breaking travis is bad.
          - we need an robust way to roll back.
             - there may be such a thing.
                - the question is, is it really worth it?
                Yes.
                  what do we get for that?
                docker vs. stack: is there a way to shrink .stack-work, ~/.stack?
                aula build depends on thentos build, which takes an hour to build with stack from scratch.


## Postmortem

We changed the thentos-core dependency for aula.
We introduce the change in a feature-branch.

Double dependency of vector, we decided not to fix in the cabal file. Use stack instead.

- We wanted to resolve two problems at once
  - Migrate docker build process from cabal to stack
  - Fix the docker image for travis


sequence of events:
- thentos-core 0.3
- docker image update to thentos-core-0.3
- new docker image breaks aula master build, which still depends on thentos-core-0.2
- aula feature branch merged to master, now aula depends on thentos-0.3
- vector library double dependency
- the travis build is broken
- decision to migrate to stack
- unforeseen issues during the migration
- slow feedback loop
- every try was done in quay.io instead of local docker builds
- desaster!


## other sources on what we are doing here.

we use quay.io because they have build machines.  and those are faster than travis, and using travis to build docker images is a misuse of the ci system.
...



## lessons

- build locally (`docker build` rather than `git push `).
- if something is broken, try the easiest thing that makes things work again.  (fix dep in cabal file rather than introducing stack).


- always merge aula

[these steps only if we make downwards-incompatible changes to thentos.]
1. push thentos
2. build docker image locally
3. test aula on local docker image
4. push docker image to quay.io
5. push aula
6. push aula-docker


[better idea.]
make the docker file smarter: it should look up the thentos version aula wants, and check out the corresponding tag in the thentos repo before building anything.
- add `RUN cd .../thentos && git checkout `cat .../aula-docker/thentos-release.txt` && .. -` to Dockerfile
- or, much easier: just use git to move the thentos git submodule to the release tag we want to have.  then quay.io will use that version.  this is the solution we should use!


### recovery

Let's assume we broke something (much less likely if we follow the thoughts outlined above).  How do we get back to a stable combination of thentos, aula, aula-docker?

- revert master on repos aula-docker, aula, thentos to a state X before everything broke.
- build aula-docker locally and docker-push it to quay.io with the 'latest' tag.


two related problems here:
    - travis is going bad.  what to do?  (solution: tag quay.io builds and pick a stable tag in .travis.yml)
    - a developer uses docker locally with thentos and aula paths and wants to revert to a working state and continue developing from there.  (solution: ?  probably related?  oh, it's simple!  "only build docker images from aula tags, not from master HEAD."  then we can revert to that tag.  same with thentos.)

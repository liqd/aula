# the aula software stack

This document summarizes some of the technical decisions made for this
project, and gives some of the reasons.

NOTE: May partially outdated.  See git commit info for last update.


# servant

Nobody in the team had any reservations.


# lucid

Alternative https://hackage.haskell.org/package/ede:

- has servant binding
- blaze looks like a monad, but isn't one
- *may* be easier to teach to HTML hackers

Alternative https://hackage.haskell.org/package/blaze-html:

- fast
- flexible
- type safe (can't miss closing brackets)
- only two languages: HTML and Haskell, not a third for templating logic
- 5 times more downloads than ede on hackage
- we already use it in thentos

lucid:

- very similar to blaze in use
- has an intact monad (even a transformer!)
- slightly tidier than blaze in many details.


## blaze is not a monad

From the blaze-html and blaze-markup packages:

> This use has its cost, as we don't support passing values inside the
> monad.  Hence, `return x >>= f != f x`. We tried supporting passing
> values, but it cost too much performance.

```haskell
    h1 >>= f = h1 >> f
        (error "Text.Blaze.Internal.MarkupM: invalid use of monadic bind")
```

https://github.com/jaspervdj/blaze-html/blob/master/doc/RFC.lhs#L199-L201
https://github.com/jaspervdj/blaze-markup/blob/486b7b1804be815369e8d154f489958d4b5f00b7/src/Text/Blaze/Internal.hs#L192-L193


# configifier

We use a compiled-in record type that contains the few things we need
to configure, and migrate to configifier when it makes sense.
(Arguably, we could go into production without allowing for config
files, as the development team will be responsible for all
deployments.)

Using configifier raises some interesting questions re. composability
of thentos-core.  In principle configifier should be as easy to use
for writing building blocks that can be composed from scratch for each
new application, but in practice this may require some refactoring.
Using two configifier types and parse them independently may be a
little brittle: we can have two independent config files, but not two
independent command lines.


# thentos-core

We use the following features from thentos:

- session state
- digestive-functions for servant
- CSRF protection

Note that some (most?) of these features should be factored out into
independent libraries, but for now they are just thentos-core.

The aula application logic will be implemented with as little mention
of any concrete thentos-core types as possible.

For instance, in order to avoid having to deal with lio, we could
define a monad 'AulaAction' and class 'MonadAulaAction' (or similar
constraint type) that implies 'MonadIO', which in turn is implemented
using 'ioTCB'.


# stm, postgresql-simple, ...

We start with a global `TVar` that contains the state and will die
every time the server is restarted.  This is enough to quickly produce
something that can be used for client communication and training.

Options for persistence (decision postponed):

- postgresql-simple
- esqueleto (together with persistent-postgresql)
- opaleye

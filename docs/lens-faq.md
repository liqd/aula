# The official Aula Lens FAQ

This document is used to distribute knowledge of the lens library
within the aula dev team.  It is hoped to be of wider usefulness, but
always rooted in aula-specific situations and examples.

Questions are losely organized by the time they arise (youngest
first).


## What is the idea of "prismatic exception types"?

Context: ?.

...


## Lenses vs. Prisms vs. Traversals

Context: PR#209.

```
data IdeaLocation =
      IdeaLocationSpace { _ideaLocationSpace :: IdeaSpace }
    | IdeaLocationTopic { _ideaLocationSpace :: IdeaSpace, _ideaLocationTopicId :: AUID Topic }

makeLenses ''IdeaLocation
```

Following the docs of https://hackage.haskell.org/package/lens-4.13/docs/Control-Lens-TH.html#v:makeLenses we get a lens to access the idea space and only a traversal to access the topic id.

```
ideaLocationSpace :: Lens' IdeaLocation IdeaSpace
ideaLocationTopicId :: Traversal' IdeaLocation (AUID Topic)
```

My guess is that this type is impossible to fulfill. `ideaLocationTopicId` cannot be:

* a getter, since there might be no `AUID Topic`
* a lens, since it cannot be a getter
* a prism, since we cannot reconstruct the `IdeaSpace` if we start only with an `AUID Topic`

What we have from my previous comment is a `Traversal' IdeaLocation (AUID Topic)`.
Looking at the main diagram https://hackage.haskell.org/package/lens-4.13 a traversal is weaker than a lens or a prism. A traversal can be turned into a setter or a fold. Assume `t` to be a
`Traversal' s a`:

* `s ^.. t` get all the elements as a list: `[a]`
* `s ^? t` get the first element: `Maybe a`
* `s & t .~ a` set all the elements to `a`.
* `s & t %~ f` modify all the elements with `f`.

Then `ideaTopicId` is the composition of `ideaLocation` (a lens) and `ideaLocationTopicId` (a traversal). However guessing the correct type signature can be difficult. My advice is to omit the type
signatures at first. Since a traversal is weaker than a lens the resulting composite is a traversal.

```
ideaTopicId :: Traversal' Idea (AUID Topic)
ideaTopicId = ideaLocation . ideaLocationTopicId
```

What you were getting at with "is it somehow ok to yield a maybe here?" is that we know that there is either no topic id or a single topic id. The traversal is weaker it can be any number of them.
A custom lens can be written as:

```
ideaLocationMaybeTopicId :: Lens' IdeaLocation (Maybe (AUID Topic))
ideaLocationMaybeTopicId f = \case
    IdeaLocationSpace spc     -> mk spc <$> f Nothing
    IdeaLocationTopic spc tid -> mk spc <$> f (Just tid)
  where
    mk spc = \case
        Nothing  -> IdeaLocationSpace spc
        Just tid -> IdeaLocationTopic spc tid
```

The composite lens is then:

```
ideaMaybeTopicId :: Lens' Idea (Maybe (AUID Topic))
ideaMaybeTopicId = ideaLocation . ideaLocationMaybeTopicId
```

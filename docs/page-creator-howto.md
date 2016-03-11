## Preparation

We assume the environment is prepared as described in README.md
under sections 'Getting started (with docker)' and 'HTML hacking'.

Resources for page creators are gathered on Taiga
(the clickdummy and some related texts at
https://tree.taiga.io/project/evpa-aula/us/20
and https://tree.taiga.io/project/evpa-aula/us/25).
A raw SVG file for the clickdummy is also available on request
(and old version is at https://tree.taiga.io/project/evpa-aula/wiki/home).
Questions for the client can be asked on Taiga, as well.

For a list of html5 elements as they are available in the Haskell
code, see
https://hackage.haskell.org/package/lucid-2.9.4/docs/Lucid-Html5.html.


## Improving pages under `samples/`

An index of all pages known to the system is at <http://localhost:8080/samples>.
Links from these pages point outside of `samples/`, but it's best to focus
on the pages with sample data for initial development.

Let's say I want to improve page

```
http://localhost:8080/samples/001_PageRoomsOverview.html-tidy.html
```

I can start by tweaking the html in

```
$AULA_SAMPLES/001_PageRoomsOverview.html-tidy.html
```

Once I make progress, I can record the change with

```
git commit -a -m "Improve 001_PageRoomsOverview"`
```

and switch to the Haskell template code that generates this page,
which can be found via `git grep "ToHtml PageRoomsOverview"`
(rarely it's something else than `ToHtml`; so ask around if `grep` fails)
and turns out to reside in

```
src/Frontend/Page/Overview.hs
```

After I tweak the `ToHtml` instance and save the file,
the `click-dummies-refresh` utility running in another
terminal automatically generates the HTML files afresh.
At this point I can compare visually and with `git diff` the two versions:
the result of the generation and my previous manually tweaked HTML.

Occasionally, I may encounter the problem that I need some information
from the system state that is not available in the current code
for the `ToHtml` instance. In our running example, it means the value
under the constructor `PageRoomsOverview` does not contain
some information I would like to display on the page.
I have two immediate options:

1. Extend the type of `PageRoomsOverview`.

2. Fake it in the Haskell code, making up some literal values,
  or introducing local fake values (`where...`) in the `toHtml` function,
  with the intention of constructing them from the value under
  constructor later on, when it's extended.

Both of these options are an improvement over faking values in the HTML files.


## Editing `samples/*.hs` (the input file).

The arbitrary data in the pages is often a bit chaotic.  If you want
to fine-tune that without having to touch the generators in aula under
`/src/Arbitrary.hs`, you can just edit the file with the name of your
page and the ending `.hs`.

If you do this, you will have to trigger `click-dummies-refresh` to refresh,
because these files aren't watched.  You can trigger `click-dummies-refresh`
either by pressing `Enter` in its terminal or by touching any file
in the aula repo.

If you run into any issues with `click-dummies-refresh`,
try to end (^D) and restart.


## Creating new pages under `samples/`

FIrst, pick a page from the clickdummy and note it down as reserved
for you at https://tree.taiga.io/project/evpa-aula/us/20.
Then create or extend a type definition for this page
(such as `PageRoomsOverview` above; the guys know where to put it),
add it to `/src/Arbitrary.hs`, `exec/RenderHtml.hs`
and `tests/Frontend/CoreSpec.hs`, create a dummy `ToHtml` instance
(and/or other instances; ask around) and fight down any type errors.
At this point the page should get listed at <http://localhost:8080/samples>.
The page is trivial, but ready for further improvement, as described
in section 'Improving pages under `samples/`'.


## Creating fully functional pages under `/` or `testing/`

Not intended for general consumption yet.


## A quick explanation of the idea of `Frontend.Page`

```
19:55 < fisx_> before my routing table desaster branch,
19:55 < fisx_> everything was under /
19:55 < fisx_> and worked.
19:55 < fisx_> then it went to /testing.
19:55 < fisx_> and stopped working, because many paths were still pointing to /
19:56 < andorp> and I don't want to introduce explicit "testing" preffix
for the pages I develop
19:56 < fisx_> then, another branch got merged that introduced Frontend.Path.
19:57 < fisx_> there you can find a constructor TopTesting.
19:57 < fisx_> if you go to your handler code or page code or whatever
that used to work before the routing table branch,
19:57 < fisx_> and find all links `"/some/where"`, an
19:57 < fisx_> *d
19:57 < fisx_> replace them with ..
19:58 < fisx_> `P.path $ P.TopTesting "/some/where"`, you should be fine.
19:58 < fisx_> things should work as before.
19:58 < fisx_> and if we decide to move /testing to /old_testing,
19:58 < fisx_> you will only have to touch the one line in Frontend.Path, where
19:58 < fisx_> the consturctor TopTesting is mapped to /testing
19:59 < fisx_> so far so good.
19:59 < fisx_> and under testing, you can make up your own page map,
and it'll work, because you can built it in a consistent way.
19:59 < fisx_> now.
19:59 < fisx_> once we move this to my desastrous new routing table,
(still exaggerating)
20:00 < fisx_> we only have to temporarily turn off TopTesting,
20:00 < fisx_> and follow the ghc errors.
20:00 < fisx_> so it will be easy to move everything into a different page map.
20:00 < fisx_> and you don't have to worry about that now.
20:00 < fisx_> the end.  :)
20:00 < fisx_> questions?
20:00 < fisx_> (probably)
20:00  * fisx_ is looking at andorp
```

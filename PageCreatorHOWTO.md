## Preparation

We assume the environment is prepared as described in README.md
under sections 'Getting started (with docker)' and 'HTML hacking'.

Resources for page creators are gathered on Taiga
(the clickdummy and textual descriptions of pages).
A raw SVG file for the clickdummy is also available
on request. Questions for the client can be asked
on Taiga, as well.


## Improving pages under `samples/`

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

and switch the Haskell template code that generates this page,
which can be found via `git grep "ToHtml PageRoomsOverview"`
and turns out to reside in

```
src/Frontend/Page/Overview.hs
```

After I tweak the `ToHtml` instance and save the file,
the `click-dummies-refresh` utility running in another
terminal automatically refreshes the HTML files
and I can compare visually and with 'git diff' the result
of the generation from the template with my previous manual
tweaks to the HTML file.

Occasionally, I may encounter the problem that I need some information
from the system state that is not available in the current code
for the `ToHtml` instance. In our running example, it means the value
under the constructor `PageRoomsOverview` does not contain
some information I would like to display on the page.

Then I have two immediate options:

1. Extend the type of `PageRoomsOverview`

2. Fake it in the Haskell code, making up some literal values,
  or introducing local fake values (`where...`) in the `toHtml` function,
  with the intention of constructing them from the value under
  constructor later on, when it's finally extended.

Both of these options are an improvement over faking values in the HTML files.


## Creating new pages under `samples/`

TODO: details

I pick a page from the list, mark it as reserved for me,
create a couple of empty files and a dummy `ToHtml` instance
(the guys know where to put it) and a new, trivial page is ready
for improvements, as described in the previous section.


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

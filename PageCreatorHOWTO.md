## Steps to create static pages

TODO: use the email and the following IRC backlog:

< mikolaj> so how can I render the page I'm working on? just look at the HTML in static/?
< fisx_> that's one good way.
< fisx_> or implement the logic behind the page and then use it as intended.
< fisx_> the latter is more work.
< mikolaj> how can I use it, not being able to display it in the browser?
< mikolaj> oh, just point the browser to some html under static/?
< fisx_> yes.
< fisx_> if you load static/samples, you get a directory
< fisx_> search for the type name there (^F)
< mikolaj> got it, a good tip
< mikolaj> and this one actually has some stuff in it already: http://localhost:8080/samples/02_PageIdeasOverview.html-compact.html
< mikolaj> fisx_: so, assuming for the sake of argument that 02_PageIdeasOverview.hs is complete, what's missing to have 02_PageIdeasOverview.html-compact.html display as in the click dummy? let's ignore for the moment how/if the links, forms, logic work
< fisx_> if you have x.hs and want x.html-compact.html to work, you need to work on the ToHtml instance of type x.
< fisx_> that's currently in Frontend.Html, but see #24
< fisx_> you will probably encounter the problem that you need some information from the system state that is not contained in values of type x.
< fisx_> then you have two options:
< fisx_> 1) extend x
< fisx_> 2) introduce local values (where...) in the toHtml function, and move them into the type later.
< fisx_> (or, of course, just make up literal values)
< fisx_> there is /static/screen.css, and you are successfully loading this.
< fisx_> (hence the dashed lines)
< fisx_> static content like images will go there.
< fisx_> avatar images that can be uploaded haven't been decided yet.
< fisx_> either there or database.


## Steps to complete the pages

TODO


## unrelated?  on pages, paths, routes.

19:54 < fisx_> perhaps i have a simple solution.
19:55 < fisx_> (my problem is that i don't really know what you are doing at the moment, so i hvae to explain in general terms.)
19:55 < fisx_> before my routing table desaster branch,
19:55 < fisx_> everything was under /
19:55 < fisx_> and worked.
19:55 < fisx_> then it went to /testing.
19:55 < andorp> It is not a disaster :)
19:55 < fisx_> and stopped working, because many paths were still pointing to /
19:56 < andorp> yes
19:56 < andorp> adn
19:56 < fisx_> (yes, i just like black&white.  and it wasn't a complete success either :)
19:56 < fisx_> i'll just finish my train of thought, but feel free to interleave your replies.
19:56 < andorp> and I don't want to introduce explicit "testing" preffix for the pages I develop
19:56 < fisx_> then, another branch got merged that introduced Frontend.Path.
19:57 < fisx_> there you can find a constructor TopTesting.
19:57 < fisx_> if you go to your handler code or page code or whatever that used to work before the routing table branch,
19:57 < fisx_> and find all links `"/some/where"`, an
19:57 < fisx_> *d
19:57 < fisx_> replace them with ..
19:58 < fisx_> `P.path $ P.TopTesting "/some/where"`, you should be fine.
19:58 < fisx_> things should work as before.
19:58 < fisx_> and if we decide to move /testing to /old_testing,
19:58 < fisx_> you will only have to touch the one line in Frontend.Path, where
19:58 < fisx_> the consturctor TopTesting is mapped to /testing
19:59 < fisx_> so far so good.
19:59 < fisx_> and under testing, you can make up your own page map, and it'll work, because you can built it in a consistent way.
19:59 < fisx_> now.
19:59 < fisx_> once we move this to my desastrous new routing table, (still exaggerating)
20:00 < fisx_> we only have to temporarily turn off TopTesting,
20:00 < fisx_> and follow the ghc errors.
20:00 < fisx_> so it will be easy to move everything into a different page map.
20:00 < fisx_> and you don't have to worry about that now.
20:00 < fisx_> the end.  :)
20:00 < fisx_> questions?
20:00 < fisx_> (probably)
20:00  * fisx_ is looking at andorp

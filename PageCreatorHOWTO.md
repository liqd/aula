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

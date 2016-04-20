// Pop Menus

var pops = document.getElementsByClassName("pop-menu");
for(p in pops) {
    if(pops[p].className) {
        addPopEvents(pops[p]);
    }
}

function addPopEvents(el) {
    el.onclick = function() {
        toggleMenu(el);
    }

    el.onmouseleave = function() {
        toggleMenu(el, true);
    }
}

function toggleMenu(el, out) {
    if(el.className.indexOf("m-open") > -1) {
        removeClass(el, "m-open");
    } else {
        if(!out) addClass(el, "m-open");
    }
}

// Mobile menu

document.getElementById("mobile-menu-button").onclick = function() {
    toggleMenu(document.getElementById("main-header"));
}

// JS detection

var body = document.getElementsByTagName("body")[0];
removeClass(body, "no-js");

// Category image selecting

var imageSelect = getElementByClassName("category-image-select");
if(imageSelect) {
    var buttons = imageSelect.getElementsByClassName("icon-list-button");
    var hidden = document.querySelectorAll("input[type=hidden]")[0];

    var handler = function(b1) {
        for (b2 = 0; b2 < buttons.length; ++b2) {
            removeClass(buttons[b2].parentNode, "m-active");
            if (b2 == b1) {
                if (hidden.value === "") {
                    addClass(buttons[b2].parentNode, "m-active");
                    hidden.value = b1;
                } else {
                    hidden.value = "";
                }
            }
        }
    };

    for (b = 0; b < buttons.length; ++b) {
        var makeHandler = function(b1) { return function() { return handler(b1); } };
        if(buttons[b] && buttons[b].className) {
            buttons[b].addEventListener("click", makeHandler(b));
        }
    }

    if (hidden.value !== "") {
        handler(hidden.value);
    }
}

// helpers

function getElementByClassName(el, parent) {
    if (typeof(parent) === "undefined") parent = document;
    return parent.getElementsByClassName(el)[0];
}

function removeClass(el, cl) {
    if(el) {
        el.className = el.className.replace(" " + cl, "");
        el.className = el.className.replace(cl, "");
    }
}

function addClass(el, cl) {
    if(el) el.className = el.className + " " + cl;
}

function reloadOnClick(hash) {
    // NOTE: it would be nice to avoid reload, but this is not a hard
    // requirement any more.
    // FIXME: this is a race condition: if we wait for 0 ms, the page
    // will usually be reloaded before the POST request updating the
    // score can be processed.
    setTimeout(function() {
        document.location.hash = hash;
        document.location.reload(true);
    }, 100);
}

function createPageSample() {
    setTimeout(function() {
        if (document.location.search === "") {
            document.location.search = "?create_page_sample=true";
        } else {
            document.location.search += "&create_page_sample=true";
        }
    }, 50);
}

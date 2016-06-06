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

    elList = getElementByClassName("pop-menu-list", el);
    elList.onmouseleave = function() {
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

// Confirm delete
function areYouSure(msg) {
    if (!msg) {
        msg = "Bitte bestätigen!";
    }
    return confirm(msg);
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

    var makeHandler = function(b1) {
        return function() {
            for (b2 = 0; b2 < buttons.length; ++b2) {
                if(buttons[b2] && buttons[b2].className) {
                    if (b2 == b1) {
                        var toggledOn = toggleClass(buttons[b2].parentNode, "m-active");
                        hidden.value = toggledOn ? b2 : "";
                    } else {
                        removeClass(buttons[b2].parentNode, "m-active");
                    }
                }
            }
        }
    };

    for (b = 0; b < buttons.length; ++b) {
        buttons[b].addEventListener("click", makeHandler(b));
    }

    if (hidden.value !== "") {
        addClass(buttons[hidden.value].parentNode, "m-active");
    }
}

// UI Messages

document.onclick = function() {
    var msg = getElementByClassName("ui-messages m-visible");
    removeClass(msg, "m-visible");
}

// Markdown

var mdEls = document.getElementsByClassName("markdown");
var converter = new showdown.Converter();
for(i in mdEls) {
    if (mdEls[i].innerHTML) {
        mdEls[i].innerHTML = converter.makeHtml(mdEls[i].innerHTML);
    }
}

// Show Markdown Preview

function showPreview(el) {
    var textEl = document.getElementById(el);
    var previewEl = document.getElementById(el + "-preview");
    updateMarkdownPreview(textEl, previewEl);
    toggleClass(previewEl, "m-closed");
    textEl.onkeyup = function() {
        if(!hasClass(previewEl, "m-closed")) {
            updateMarkdownPreview(textEl, previewEl);
        }
    };
}

function updateMarkdownPreview(textEl, previewEl) {
    if (textEl.value.trim() == "") {
        previewEl.innerHTML = "[Text field is empty]";
    } else {
        previewEl.innerHTML = converter.makeHtml(textEl.value);
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

function hasClass(el, cl) {
    if(el) return (el.className.indexOf(cl) > -1);
}

function toggleClass(el, cl) {
    if(el) {
        var old = el.className;
        removeClass(el, cl);
        if (el.className === old) {
            addClass(el, cl);
            return true;
        } else {
            return false;
        }
    }
}

var reloadDelayMs = 250;

function reloadOnClick(target) {
    // NOTE: it would be nice to avoid reload, but this is not a hard
    // requirement any more.
    // FIXME: this is a race condition: if we wait for 0 ms, the page
    // will usually be reloaded before the POST request updating the
    // score can be processed.
    setTimeout(function() {
        if (target && target.hash) {
            document.location.hash = target.hash;
        }
        if (target && target.href) {
            document.location.href = target.href;
        }
        document.location.reload(true);
    }, reloadDelayMs);
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

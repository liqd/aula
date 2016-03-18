// Pop Menus
var pops = document.getElementsByClassName("pop-menu");
for(p in pops) {
    if(pops[p].className) {
        addPopEvents(pops[p]);
    }
}

// JS detection
var body = document.getElementsByTagName("body")[0];
removeClass(body, "no-js");

// Category image selecting
var imageSelect = getElementByClassName("category-image-select");
if(imageSelect) {
    var buttons = imageSelect.getElementsByClassName("icon-list-button");
    var radiosContainer = getElementByClassName("category-radios");
    var radios = radiosContainer.getElementsByTagName("input");

    var handler = function(b1) {
        for (b2 = 0; b2 < buttons.length; ++b2) {
            if (b2 == b1) {
                addClass(buttons[b2], "m-active");
            } else {
                removeClass(buttons[b2], "m-active");
            }
        }
        radios[b1].checked = true;  // this sets all other radios to false.
    };

    for (b = 0; b < buttons.length; ++b) {
        var makeHandler = function(b1) { return function() { return handler(b1); } };
        if(buttons[b] && buttons[b].className) {
            buttons[b].addEventListener('click', makeHandler(b));
        }
    }
}

function addPopEvents(el) {
    el.onmouseenter = function() {
        toggleMenu(el);
    }

    el.onmouseleave = function() {
        toggleMenu(el);
    }
}


function toggleMenu(el) {
    if(el.className.indexOf("m-open") > -1) {
        removeClass(el, "m-open");
    } else {
        addClass(el, "m-open");
    }
}

function getElementByClassName(el, parent) {
    if (typeof(parent)==='undefined') parent = document;
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

function endsWith(str, suffix) {
    return str.indexOf(suffix, str.length - suffix.length) !== -1;
}

// Pop Menus
var pops = document.getElementsByClassName("pop-menu");
for(p in pops) {
    if(pops[p].className) {
        addPopEvents(pops[p]);
    }
}

//JS detection
var body = document.getElementsByTagName("body")[0];
removeClass(body, "no-js");

//Category image selecting
var imageSelect = getElementByClassName("category-image-select");
if(imageSelect) {
    var buttons = getElementByClassName("icon-list-button");
    var radiosContainer = getElementByClassName("category-radios");
    var radios = radiosContainer.getElementsByTagName("input");

    for(b in buttons) {
        if(buttons[b]) if(buttons[b].className) buttons[b].addEventListener('click', function(el) {
            var id = el.target.id.replace("select-", "");
            var radioEl = document.getElementById(id);
            deselectAllCategories();
            radioEl.setAttribute("checked","checked");
            addClass(el.target, "m-active");
        });
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

function deselectAllCategories() {
    for(r in radios) {
        if(radios[r].id) {
            radios[r].removeAttribute("checked");
            removeClass(buttons[r], "m-active");
        }
    }
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
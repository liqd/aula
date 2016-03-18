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

    for(b = 0; b < buttons.length; ++b) {
        if(buttons[b] && buttons[b].className)
            buttons[b].addEventListener('click', function(el) {
                var categoryid = el.target.id.replace("select-", "");
                console.log("categoryid=" + categoryid);
                deselectAllCategories();
                selectCategory(categoryid)
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
    for(r=0; r<radios.length; ++r) {
        if(radios[r].id) {
            radios[r].checked = false;
            removeClass(buttons[r], "m-active");
        }
    }
}

function selectCategory(categoryid) {
    for(r=0; r<radios.length; ++r) {
        if(radios[r].id && endsWith(String(radios[r].id), String(categoryid))) {
            radios[r].checked = true;
            addClass(buttons[r], "m-active");
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
function endsWith(str, suffix) {
    return str.indexOf(suffix, str.length - suffix.length) !== -1;
}

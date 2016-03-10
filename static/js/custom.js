// Header dropdown
var drop = document.getElementsByClassName("main-header-user")[0];
drop.onclick = function() {
    toggleMenu();
};

function toggleMenu() {
    if(drop.className.indexOf("m-open") > -1) {
        removeClass(drop, "m-open");
    } else {
        addClass(drop, "m-open");
    }
}

var menu = document.getElementsByClassName("pop-menu-list")[0];
menu.onmouseleave = function() {
    toggleMenu();
}

//JS detection
var body = document.getElementsByTagName("body")[0];
removeClass(body, "no-js");

//Category image selecting
var imageSelect = document.getElementsByClassName("category-image-select")[0];
if(imageSelect) {
    var buttons = document.getElementsByClassName("icon-list-button");
    var radiosContainer = document.getElementsByClassName("category-radios")[0];
    var radios = radiosContainer.getElementsByTagName("input");

    for(b in buttons) {
        if(buttons[b].className) buttons[b].addEventListener('click', function(el) {
            var id = el.target.id.replace("select-", "");
            var radioEl = document.getElementById(id);
            deselectAllCategories();
            radioEl.setAttribute("checked","checked");
            addClass(el.target, "m-active");
        });
    }
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
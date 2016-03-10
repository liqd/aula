// Header dropdown
var drop = document.getElementsByClassName("main-header-user")[0];
drop.onclick = function() {
    toggleMenu();
};

function toggleMenu() {
    if(drop.className.indexOf("m-open") > -1) {
        drop.className = drop.className.replace(" m-open", "");
        drop.className = drop.className.replace("m-open", "");
    } else {
        drop.className = drop.className + " m-open";
    }
}

var menu = document.getElementsByClassName("pop-menu-list")[0];
menu.onmouseleave = function() {
    toggleMenu();
}

//JS detection
body = document.getElementsByTagName("body")[0];
body.className = body.className.replace("no-js", "");

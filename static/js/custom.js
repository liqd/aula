// Header dropdown
var drop = document.getElementsByClassName("main-header-user")[0];
drop.onclick = function() {
    if(drop.className.indexOf("m-open") > -1) {
        drop.className = drop.className.replace(" m-open", "");
        drop.className = drop.className.replace("m-open", "");
    } else {
        drop.className = drop.className + " m-open";
    }
};

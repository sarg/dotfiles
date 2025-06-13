var filter = {};

function applyFilters() {
    document.querySelectorAll(".ratings [data-tags]")
        .forEach(el => el.hidden = Object.values(filter).find(f => !f(el)));
}

document.querySelectorAll("ul.tags > li").forEach(el => el.onclick = function(event) {
    var active = event.srcElement.classList.toggle('active');

    var tagName = event.srcElement.innerText;
    var tag = event.srcElement.getAttribute('tag');
    if (active) {
        var rx = new RegExp('\\b' + tag + '\\b');
        filter[tag] = el => rx.test(el.getAttribute('data-tags'));
    } else {
        delete filter[tag];
    }

    history.pushState({}, "", '#' + Object.keys(filter).join(","));
    applyFilters();
});

window.top.location.hash.substr(1).split(",")
    .forEach(t => document.querySelectorAll("[tag='" + t + "']").forEach(el => el.click()));

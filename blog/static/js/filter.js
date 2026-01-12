var filter = {};

function applyFilters() {
    document.querySelectorAll(".ratings [data-tags]")
        .forEach(el => el.hidden = Object.values(filter).find(f => !f(el)));
}

function clickTag(event) {
    var active = event.srcElement.classList.toggle('active');
    var tag = event.srcElement.getAttribute('tag');
    if (active) {
        var rx = new RegExp('\\b' + tag + '\\b');
        filter[tag] = el => rx.test(el.getAttribute('data-tags'));
    } else {
        delete filter[tag];
    }

    history.pushState({}, "", '#' + Object.keys(filter).join(","));
    applyFilters();
}

for (const el of tags.children) {
    el.onclick = clickTag;
}

window.top.location.hash.substr(1).split(",")
    .forEach(t => tags.querySelector("[tag='" + t + "']").click());

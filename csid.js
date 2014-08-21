var reveal = false;

function getSettings(name) {
  var settings = $.cookie(name);
  var data = [];
  if (settings)
    data = settings.split(",");
  return data;
}

var lastVenue = false;

function addNavigation() {
  var deniedVenues = getSettings("deniedVenues");
  var shows = getSettings("shows");

  var added = 0;
  $("tr").each(function(key, node) {
    var name = node.getAttribute("name");
    if (! name)
      return;

    if (! $("input#" + name)[0]) {
      var checked = "";
      if (deniedVenues.indexOf(name) == -1)
	checked = "checked";
      $("#selector").append("<span class='venue'><input type=checkbox " + 
			    checked + " id='" + name + "'><span id='venue-" +
			    name + "'>" +
			    name.replace("_", " ") + "</span></span>");
      $("#" + name).bind("click", function(e) {
	hideShow();
	setVenueCookie();
      });
      $("#venue-" + name).bind("click", function(e) {
	fixPosition();
	if (lastVenue != name) {
	  hideShow(name);
	  lastVenue = name;
	} else {
	  hideShow();
	  lastVenue = false;
	}
      });
    }

    $(node).children("td").each(function(key, td) {
      $(td).bind("click", function() {
	top.location.href = $(node).find("a").attr("href");
      });
    });
    
    var id = node.id.replace("event-", "");
    $(node).append("<td class=show><input type=checkbox id='show-" + id + 
		   "' " + 
		   (shows.indexOf(id) == -1? "": "checked") +
		   ">");
    if (shows.indexOf(id) != -1)
      $("#event-" + id).addClass("checked");
    $("#show-" + id).bind("click", function(e) {
      toggleShow(id, this.checked);
    });
  });

  // Sort all the venues.
  $("#selector")
    .children("span")
    .sort(function(a, b) {
      return $(a).find("span").attr("id")
	.localeCompare($(b).find("span").attr("id"));
    })
    .detach()
    .appendTo("#selector");
  
  hideShow();
}

function hideShow(onlyVenue) {
  var venues = [];
  var i = 0;
  $("input[type=checkbox]").each(function(key, node) {
    if (node.checked && ! node.id.match(/show/))
      venues[i++] = node.id;
  });
  var prevDate = "";
  $("tr").each(function(key, node) {
    var name = node.getAttribute("name");
    if (! name)
      return;
    if ((onlyVenue && name == onlyVenue) ||
	(! onlyVenue && venues.indexOf(name) != -1)) {
      $(node).removeClass("invisible");
      // Make just a single date field per day visible.
      $(node).prev().children("td").each(function(key, date) {
	var text = date.innerHTML;
	if (text != prevDate)
	  $(date).parent().removeClass("invisible");
	else 
	  $(date).parent().addClass("invisible");
	prevDate = text;
      });
    } else {
      $(node).addClass("invisible");
      $(node).prev().addClass("invisible");
    }
  });
}

function removeElement(arr, val) {
  for (var i = 0; i < arr.length; i++) {
    if (arr[i] === val) {
      arr.splice(i, 1);
      i--;
    }
  }
  return arr;
}

function toggleShow(id, checked) {
  var shows = getSettings("shows");
  if (checked) {
    if (shows.indexOf(id) == -1)
      shows.push(id);
    $("#event-" + id).addClass("checked");
  } else {
    shows = removeElement(shows, id);
    $("#event-" + id).removeClass("checked");
  }
  $.cookie("shows", shows.join(), { expires: 10000 });
}

function setVenueCookie() {
  var venues = [];
  var i = 0;
  $("input[type=checkbox]").each(function(key, node) {
    if (! node.id.match(/show/) && ! node.checked)
      venues[i++] = node.id;
  });
  $.cookie("deniedVenues", venues.join(), { expires: 10000 });
}

function fixPosition() {
  console.log("fixing");
  $("#body-container").each(function(key, body) {
    var pos = $(body).offset();
    body.style.position = "absolute";
    body.style.left = pos.left + "px";
    body.style.top = pos.top + "px";
  });
}

addNavigation();

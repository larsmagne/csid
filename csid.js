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

  $("tr").each(function(key, node) {
    var name = node.getAttribute("name");
    if (! name)
      return;

    if (! document.getElementById(name)) {
      var checked = "";
      if ($.inArray(deniedVenues, name) == -1)
	checked = "checked";
      $("#selector").append("<span class='venue'><input type=checkbox " + 
			    checked + " id='" + name + "'><span id='venue-" +
			    name + "' class='venue-name'>" +
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
		   ($.inArray(shows, id) == -1? "": "checked") +
		   ">");
    if ($.inArray(shows, id) != -1)
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

  $("#selector").append("<a class='export'>Export your chosen show list</a>");
  $(".export").bind("click", function(e) {
    exportShows();
  });

  if (window.location.href.match("shows=")) {
    $("#selector").append(" - <a class='clear'>Clear the show list</a>");
    $(".clear").bind("click", function(e) {
      window.location.href = window.location.href.replace(/[?].*/, "");
      hideShow();
    });
  }
  
  hideShow();
}

function hideShow(onlyVenue) {
  var venues = [];
  var i = 0;
  var onlyShows = window.location.href.match("shows=([0-9,]+)");
  var prevDate = false, anyVisible = false;

  // We've gotten an URL with a show list from somebody.
  if (onlyShows)
    onlyShows = onlyShows[1].split(",");

  $("input[type=checkbox]").each(function(key, node) {
    if (node.checked && ! node.id.match(/show/))
      venues[i++] = node.id;
  });

  $("tr").each(function(key, node) {
    var name = node.getAttribute("name");
    var visible;

    // Don't show date lines for dates where we're not displaying any
    // shows.
    if (! name) {
      if (prevDate) {
	if (anyVisible)
	  $(prevDate).removeClass("invisible");
	else
	  $(prevDate).addClass("invisible");
      }
      prevDate = node;
      anyVisible = false;
      return;
    }

    var match = node.id.match("event-(.*)");
    var eventId = match[1];
    
    if (onlyVenue)
      visible = name == onlyVenue;
    else if (onlyShows)
      visible = $.inArray(onlyShows, eventId) != -1;
    else
      visible = $.inArray(venues, name) != -1;
    
    if (visible) {
      $(node).removeClass("invisible");
      anyVisible = true;
    } else
      $(node).addClass("invisible");
  });

  if (prevDate) {
    if (anyVisible)
      $(prevDate).removeClass("invisible");
    else
      $(prevDate).addClass("invisible");
  }
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
    if ($.inArray(shows, id) == -1)
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
  $("#body-container").each(function(key, body) {
    var pos = $(body).offset();
    body.style.position = "absolute";
    body.style.left = pos.left + "px";
    body.style.top = pos.top + "px";
  });
}

function exportShows() {
  var shows = getSettings("shows");
  var visible = [];
  $.map(shows, function(elem) {
    if ($("#event-" + elem)[0])
      visible.push(elem);
  });
  window.location.href = window.location.href.replace(/[?].*/, "") +
    "?shows=" + visible.join();
}

addNavigation();

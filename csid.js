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
  // Default to not showing quizes.
  if (typeof $.cookie("deniedVenues") == 'undefined')
    $.cookie("deniedVenues", "Quiz");

  var deniedVenues = getSettings("deniedVenues");
  var shows = getSettings("shows");

  $("#selector").append("<div class='explanation'>Everything in <a id='help' href='help.html?1'><b>bold</b></a> is clickable</div>");

  $("tr").each(function(key, node) {
    var name = node.getAttribute("name");
    if (! name)
      return;

    if (! document.getElementById(name))
      addVenue(name, deniedVenues);

    $(node).children("td").first().bind("click", function(e) {
      if (! e.ctrlKey) {
	if ($("body").width() < 600) {
	  actionEventMenu(node);
	  return false;
	} else
	  top.location.href = $(node).find("a").attr("href");
      }
      return true;
    });

    $(node).children("td").last().bind("click", function() {
      fixPosition();
      if ($("body").width() < 600) {
	actionVenueMenu(name);
	return false;
      } else {
	if (lastVenue != name) {
	  hideShow(name);
	  lastVenue = name;
	} else {
	  hideShow();
	  lastVenue = false;
	}
      }
      return true;
    });
    
    var id = node.id.replace("event-", "");
    $(node).append("<td class=show><input type=checkbox id='show-" + id + 
		   "' " + 
		   ($.inArray(id, shows) == -1? "": "checked") +
		   ">");
    if ($.inArray(id, shows) != -1)
      $("#event-" + id).addClass("checked");
    $("#show-" + id).bind("click", function(e) {
      toggleShow(id, this.checked);
    });
  });

  // Add a virtual "quiz" venue.
  addVenue("Quiz", deniedVenues);

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
  
  var visible = "invisible";
  if ($("tr.checked").length > 0 || window.location.href.match("shows="))
    visible = "";
  $("#selector").append("<div id='export' class='export " + visible + 
		       "'><a class='export'>List only chosen events</a></div>");
  $("a.export").bind("click", function(e) {
    exportShows();
  });

  $("#selector").append("<div class='export'><a id='sort'>List event in scan order</a></div>");
  $("#sort").bind("click", function() {
    sortByScanOrder();
    addRestoreLink();
  });

  $("#selector").append("<div class='export " + visible +
			"'><a id='ical'>Export calendar</a></div>");
  $("#ical").bind("click", function() {
    exportCalendar();
  });

  if (window.location.href.match("shows=")) {
    $("#export").append(" - <a class='clear'>Clear the event list</a>");
    $("a.clear").bind("click", function(e) {
      window.location.href = window.location.href.replace(/[?].*/, "");
    });
  }

  $("#selector").append("<div class='export'><a id='rss' href='csid.atom'>Atom/\RSS feed</a></div>");

  $("img#logo").bind("click", function() {
    window.location.href = "http://csid.no/";
  });

  $('a#help').colorbox({width: "400px",
			initialWidth: "400px",
			close: "Close",
			className: "lightbox"
		       });

  $('#small-menu').bind("click", function(e) {
    showVenueChooser();
    return false;
  });

  if ($("body").width() < 600)
    loadLogos();
}

function addVenue(name, deniedVenues) {
  var checked = "";
  if ($.inArray(name, deniedVenues) == -1)
    checked = "checked";
  $("#selector").append("<span class='venue'><input type=checkbox " + 
			checked + " id='" + name + "'><span id='venue-" +
			name + "' class='venue-name'>" +
			name.replace(/_/g, " ") + "</span></span>");
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

function hideShow(onlyVenue, onlyAfterTimestamp) {
  var venues = [];
  var i = 0;
  var onlyShows = window.location.href.match("shows=([0-9,]+)");
  var prevDate = false, anyVisible = false;
  var maxTimestamp = "";

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

    var timestamp = node.getAttribute("time");
    if (timestamp > maxTimestamp)
      maxTimestamp = timestamp;

    var match = node.id.match("event-(.*)");
    var eventId = match[1];
    
    if (onlyVenue) {
      if (onlyVenue == "Quiz")
	visible = $(node).text().match(/quiz/i);
      else
	visible = name == onlyVenue;
    } else if (onlyAfterTimestamp)
      visible = timestamp > onlyAfterTimestamp;
    else if (onlyShows)
      visible = $.inArray(eventId, onlyShows) != -1;
    else {
      visible = $.inArray(name, venues) != -1;
      if (visible &&
	  $.inArray("Quiz", venues) == -1 &&
	  $(node).text().match(/quiz/i))
	visible = false;
    }
    
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

  if (! onlyAfterTimestamp) {
    if (typeof $.cookie("timestamp") == 'undefined')
      $.cookie("timestamp", maxTimestamp, { expires: 10000 });
    else if (maxTimestamp > $.cookie("timestamp") &&
	     ! document.getElementById("new")) {
      $("#selector").append("<div class='export'><a id='new'>Display events arrived since last time</a></div>");
      $("#new").bind("click", function() {
	fixPosition();
	hideShow(false, $.cookie("timestamp"));
	$.cookie("timestamp", maxTimestamp, { expires: 10000 });
	addRestoreLink();
      });
    }
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
    if ($.inArray(id, shows) == -1)
      shows.push(id);
    $("#event-" + id).addClass("checked");
    $("#export").removeClass("invisible");
    $("#ical").removeClass("invisible");
  } else {
    shows = removeElement(shows, id);
    $("#event-" + id).removeClass("checked");
    if ($("tr.checked").length == 0) {
      $("#export").addClass("invisible");
      $("#ical").addClass("invisible");
    }
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
  if ($("body").width() < 600)
    return;

  $("#body-container").each(function(key, body) {
    var pos = $(body).offset();
    body.style.position = "absolute";
    body.style.left = pos.left + "px";
    body.style.top = pos.top + "px";
  });
  $("table").each(function (key, table) {
    table.width = table.offsetWidth + "px";
    $(table).find("colgroup").children("col").each(function(key, col) {
      col.width = col.offsetWidth + "px";
    });
  });
}

function exportShows() {
  var shows = getSettings("shows");
  var visible = [];
  $.map(shows, function(elem) {
    var id = "#event-" + elem;
    if ($(id)[0] && $(id).is(":visible") )
      visible.push(elem);
  });
  window.location.href = window.location.href.replace(/[?].*/, "") +
    "?shows=" + visible.join();
}

function sortByScanOrder() {
  $trs = $("table").find("tr:not(.date)");
  $table = $("table");
  $("tr").remove();

  $trs
    .sort(function(a, b) {
      return parseInt($(a).attr("data")) - parseInt(($(b).attr("data")));
    })
    .appendTo($table);
}

function addRestoreLink() {
  $("#selector").append("<div class='export'><a href='http://csid.no/'>Restore list</a></div>");
}

function exportCalendar() {
  var shows = getSettings("shows");
  var cal = "BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN\n";
  $.map(shows, function(elem) {
    var id = "#event-" + elem;
    var $line = $(id);
    if ($line[0]) {
      var date = $line.attr("date").replace(/-/g, "");
      var venue = $line.find("td").eq(1).text();
      var band = $line.find("td").first().text();
      var start = date + "T200000";
      var end = date + "T230000";
      cal += "BEGIN:VEVENT\nUID:event-" + elem + "@csid.no\n";
      cal += "DTSTAMP:" + start + "Z\n";
      cal += "ORGANIZER;CN=" + venue + ":MAILTO:csid@example.com\n";
      cal += "DTSTART:" + start + "Z\n";
      cal += "DTEND:" + end + "Z\n";
      cal += "LOCATION:" + venue + "\n";
      cal += "SUMMARY:" + band + "\nEND:VEVENT\n";
    }
  });

  cal += "END:VCALENDAR\n";

  var blob = new Blob([cal], {
    type: "text/calendar;charset=utf8;"
  });
  saveAs(blob, "csid.ics");
}

function actionEventMenu(node) {
  var link = $(node).find("a").attr("href");
  var idString = $(node).find("input").attr("id");
  var match = idString.match(/([0-9]+)/);
  var shows = getSettings("shows");
  var id = match[1];
  var type = "Mark";
  if ($.inArray(id, shows) != -1)
    type = "Unmark";
  $.colorbox({html: "<a href='" + link + "'>Go to the event web page</a><a href='#' id='mark-event'>" + type + " event</a>",
	      width: "100%",
	      close: "Close",
	      transition: "none",
	      className: "event-lightbox"});
  $("#mark-event").bind("click", function() {
    toggleShow(id, $.inArray(id, shows) == -1);
    $.colorbox.close();
    return false;
  });
}

function actionVenueMenu(name) {
  var displayName = name.replace(/_/g, " ");
  var limit = "Just show events from " + displayName;
  if (lastVenue == name)
    limit = "Show all events again";
  var deniedVenues = getSettings("deniedVenues");
  var venues = "Don't show events from " + displayName;
  if ($.inArray(name, deniedVenues) != -1)
    venues = "Include events from " + displayName;

  $.colorbox({html: "<div class='outer-venue-logo'><img src='logos/larger/" + name + ".png'></div><a href='#' id='venue-limit'>" + limit + "</a><a href='#' id='venue-mark'>" + venues + "</a><a href='#' id='all-venues'>Show all events from all venues</a><a href='#' id='csid-close'>Close</a>",
	      width: $("body").width() + "px",
	      closeButton: false,
	      transition: "none",
	      height: "100%",
	      className: "event-lightbox"});
  $("#venue-limit").bind("click", function() {
    if (lastVenue != name) {
      hideShow(name);
      lastVenue = name;
    } else {
      hideShow();
      lastVenue = false;
    }
    $.colorbox.close();
    return false;
  });

  $("#csid-close").bind("click", function() {
    $.colorbox.close();
    return false;
  });
  
  $("#venue-mark").bind("click", function() {
    var deniedVenues = getSettings("deniedVenues");
    document.getElementById(name).checked =
      ($.inArray(name, deniedVenues) != -1);
    setVenueCookie();
    hideShow();
    $.colorbox.close();
    return false;
  });

  $("#all-venues").bind("click", function() {
    $("tr.invisible").each(function(key, node) {
      $(node).removeClass("invisible");
    });
    $.colorbox.close();
    return false;
  });
}

function showVenueChooser() {
  var venues = "<div class='venue'>Choose venues to include or exclude</div>";
  var deniedVenues = getSettings("deniedVenues");
  $("#selector").find("span.venue-name").each(function(key, node) {
    var id = node.id.replace(/venue-/, "");
    var className = "checked";
    if ($.inArray(id, deniedVenues) != -1)
      className = "unchecked";
    venues += "<div class='venue " + className + "' data='" + id +
      "'>" + node.innerHTML + "</div>";
  });
  $.colorbox({html: venues,
	      width: $("body").width() + "px",
	      close: "Close",
	      transition: "none",
	      className: "event-lightbox"});
  $("div.venue").bind("click", function() {
    var id = $(this).attr("data");
    var deniedVenues = getSettings("deniedVenues");
    $(this).removeClass("checked");
    $(this).removeClass("unchecked");
    if ($.inArray(id, deniedVenues) == -1)
      $(this).addClass("unchecked");
    else
      $(this).addClass("checked");
    document.getElementById(id).checked =
      ($.inArray(id, deniedVenues) != -1);
    setVenueCookie();
    hideShow();
    return false;
  });
}

function loadLogos() {
  var venues = [];
  var deniedVenues = getSettings("deniedVenues");
  $("#selector").find("span.venue-name").each(function(key, node) {
    var id = node.id.replace(/venue-/, "");
    if ($.inArray(id, deniedVenues) == -1)
      venues.push(id);
  });
  loadLogo(venues, 0);
}

function loadLogo(venues, index) {
  var venue = venues[index];
  var img = $("<img />").attr("src", "logos/thumb/" + venue + ".png")
	.on("load", function() {
          if (this.complete && typeof this.naturalWidth != "undefined" &&
	      this.naturalWidth != 0) {
	    var image = this;
            $("tr[name=" + venue + "]").each(function(key, node) {
	      var td = node.childNodes[1];
	      td.innerHTML = "";
	      td.className = "thumb-logo";
	      td.appendChild(image.cloneNode());
	    });
          }
	  if (index < (venues.length - 1))
	    loadLogo(venues, index + 1);
	}).error(function() {
	  if (index < (venues.length - 1))
	    loadLogo(venues, index + 1);
	});
}

addNavigation();

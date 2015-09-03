var reveal = false;
var phoneGap = false;

function getSettings(name) {
  var settings = $.cookie(name);
  if (phoneGap)
    settings = getCookie(name);
  var data = [];
  if (settings)
    data = settings.split(",");
  return data;
}

function setSettings(name, value) {
  if (phoneGap)
    setCookie(name, value, 10000);
  else
    $.cookie(name, value);
}

var lastVenue = false;

function addNavigation() {
  // Default to not showing quizes.
  //var defaultDenied = "Quiz,Buckleys,Herr Nilsen,Konserthuset,NB,Olsen,Per på hjørnet,Riksscenen,UiO";
  var defaultDenied = "Quiz";
  if (phoneGap) {
    if (getSettings("deniedVenues") == "")
      setSettings("deniedVenues", defaultDenied);
  } else {
    if (typeof $.cookie("deniedVenues") == 'undefined')
      setSettings("deniedVenues", defaultDenied);
  }

  var mobilep = phoneGap || $("body").width() < 600;
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
	if (mobilep) {
	  actionEventMenu(node, name);
	  return false;
	} else
	  top.location.href = $(node).find("a").attr("href");
      }
      return true;
    });

    if (mobilep) {
      $(node).children("td").last().bind("click", function() {
	actionVenueMenu(name);
	return false;
      });
    } else {
      $(node).children("td").last().bind("click", function() {
	fixPosition();
	if (lastVenue != name) {
	  hideShow(name);
	  lastVenue = name;
	} else {
	  hideShow();
	  lastVenue = false;
	}
	return true;
      });
    }
    
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

  if (mobilep)
    loadLogos(mobilep);
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

  hideDuplicates();
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
  setSettings("shows", shows.join());
}

function setVenueCookie() {
  var venues = [];
  var i = 0;
  $("input[type=checkbox]").each(function(key, node) {
    if (! node.id.match(/show/) && ! node.checked)
      venues[i++] = node.id;
  });
  setSettings("deniedVenues", venues.join());
}

function fixPosition() {
  if (phoneGap || $("body").width() < 600)
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

function actionEventMenu(node, venue) {
  var link = $(node).find("a").attr("href");
  var idString = $(node).find("input").attr("id");
  var match = idString.match(/([0-9]+)/);
  var shows = getSettings("shows");
  var id = match[1];
  var type = "I'm going";
  if ($.inArray(id, shows) != -1)
    type = "I'm not going after all";
  $.colorbox({html: "<div class='outer-venue-logo'><img src='logos/larger/" + fixName(venue) + ".png'></div><a id='event-link' href='" + link + "'>Display the event web page</a><a href='#' id='mark-event'>" + type + "</a><a href='#' id='csid-close'>Close</a>",
	      width: "100%",
	      closeButton: false,
	      transition: "none",
	      height: "100%",
	      className: "event-lightbox"});
  $("#mark-event").bind("click", function() {
    toggleShow(id, $.inArray(id, shows) == -1);
    $.colorbox.close();
    return false;
  });
  $("#csid-close").bind("click", function() {
    $.colorbox.close();
    return false;
  });
  $("#event-link").bind("click", function() {
    $.colorbox.close();
    document.location.href = this.href;
    return false;
  });
  $("#cboxLoadedContent").bind("click", function() {
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

  $.colorbox({html: "<div class='outer-venue-logo'><img src='logos/larger/" + fixName(name) + ".png'></div><a href='#' id='venue-limit'>" + limit + "</a><a href='#' id='venue-mark'>" + venues + "</a><a href='#' id='all-venues'>Show all events from all venues</a><a href='#' id='csid-close'>Close</a>",
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
  
  $("#cboxLoadedContent").bind("click", function() {
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
  var venues = "<div class='venue-top'>Tap venues to include or exclude</div>";
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
  $("div.venue-top").bind("click", function() {
    $.colorbox.close();
    return false;
  });
}

function loadLogos(mobilep) {
  var venues = [];
  var deniedVenues = getSettings("deniedVenues");
  $("#selector").find("span.venue-name").each(function(key, node) {
    var id = node.id.replace(/venue-/, "");
    if (phoneGap || $.inArray(id, deniedVenues) == -1)
      venues.push(id);
  });
  loadLogo(mobilep, venues, 0);
}

function loadLogo(mobilep, venues, index) {
  var venue = venues[index];
  var image = new Image();
  image.onload = function() {
    $("tr[name=" + venue + "]").each(function(key, node) {
      var td = node.childNodes[1];
      td.title = td.innerHTML;
      td.innerHTML = "";
      td.className = "thumb-logo";
      td.appendChild(image.cloneNode());
    });
    if (index < (venues.length - 1))
      loadLogo(mobilep, venues, index + 1);
  };
  image.onerror = function() {
    if (index < (venues.length - 1))
      loadLogo(mobilep, venues, index + 1);
  };
  image.src = "logos/thumb/" + fixName(venue) + ".png";
}

function hideDuplicates() {
  var seen = [];
  var shows = getSettings("shows");
  $("tr").each(function(key, node) {
    var id = node.id.replace(/event-/, "");
    if (! id)
      return;
    var text = node.childNodes[0].childNodes[0].innerHTML;
    if (! text)
      return;
    // Get rid of whitespace differences.
    text = text.replace(/ +/g, "");
    var eventKey = node.getAttribute("name") + "." + node.getAttribute("date") +
	  "." + text;
    if (seen[eventKey] && $.inArray(id, shows) == -1)
      $(node).addClass("invisible");
    seen[eventKey] = true;
  });
}

function fixName(name) {
  return name.replace(/[^A-Za-z0-9_]/g, "x");
}

function getCookie(c_name) {
    return localStorage.getItem(c_name);
}

function setCookie(c_name, value, expiredays) {
    return localStorage.setItem(c_name, value);
}

var reveal = false;
var phoneGap = false;
var sortOrder = "date";
var savedTable = false;

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
    miscMenu();
    return false;
  });

  if (mobilep) {
    if (phoneGap)
      addLogos();
    else
      loadLogos(mobilep);
    $(window).on("orientationchange", function() {
      $.colorbox.close();
      if (phoneGap)
	setHardWidths();
      return true;
    });
    if (phoneGap) {
      setHardWidths();
      StatusBar.overlaysWebView(false);
    }
  }
  if (! savedTable)
    savedTable = $("table").clone({withDataAndEvents: true});
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

function hideShow(onlyVenue, onlyAfterTimestamp, onlyEvent,
		  onlyShowsArray) {
  var venues = [];
  var i = 0;
  var onlyShows = window.location.href.match("shows=([0-9,]+)");
  var prevDate = false, anyVisible = false;
  var maxTimestamp = "";
  var blankTable = true;

  // We've gotten an URL with a show list from somebody.
  if (onlyShowsArray)
    onlyShows = onlyShowsArray;
  else if (onlyShows)
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
    } else if (onlyEvent) {
      visible = $(node).text().match(new RegExp(onlyEvent, "i"));
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
      blankTable = false;
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
  return blankTable;
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
      var end = date + "T210000";
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
  var exportString = "";
  if (phoneGap)
    exportString = "<a href='#' id='export-event'>Export Event to Calendar</a><a href='#' id='share-event'>Share Event</a>";
  colorbox("<div class='outer-venue-logo'><img src='logos/larger/" +
	   fixName(venue) + ".png'></div><div class='event-text'><div>" +
	   $(node).find("a")[0].innerHTML +
	   "</div></div><a id='event-link' href='" + link +
	   "'>Display the event web page</a><a href='#' id='mark-event'>" +
	   type + "</a>" + exportString +
	   "<a href='#' id='csid-close'>Close</a>");
  $("#mark-event").bind("click", function() {
    toggleShow(id, $.inArray(id, shows) == -1);
    $.colorbox.close();
    return false;
  });
  $("#export-event").bind("click", function() {
    $.colorbox.close();
    exportEvent(id);
    return false;
  });
  $("#share-event").bind("click", function() {
    $.colorbox.close();
    shareEvent(id);
    return false;
  });
  $("#event-link").bind("click", function() {
    $.colorbox.close();
    if (phoneGap && device.platform == "iOS")
      window.open(this.href, "_system", "location=no");
    else
      document.location.href = this.href;
    return false;
  });
  addScrollActions();
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

  colorbox("<div class='outer-venue-logo'><img src='logos/larger/" +
	   fixName(name) + ".png'></div><a href='#' id='venue-limit'>" +
	   limit + "</a><a href='#' id='venue-mark'>" + venues +
	   "</a><a href='#' id='all-venues'>Show all events from all venues</a><a href='#' id='csid-close'>Close</a>");
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
  addScrollActions();
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
  venues += "<a href='#' id='csid-close'>Close</a>";
  colorbox(venues);
  $("table").hide();
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
  var func = function() {
    $("table").show();
    $.colorbox.close();
    return false;
  };
  $("div.venue-top").bind("click", func);
  $("#csid-close").bind("click", func);
  removeScrollActions();
}

function addLogos() {
  $("tr").each(function(key, node) {
    var venue = node.getAttribute("name");
    if (! venue)
      return;
    var td = node.childNodes[1];
    td.title = td.innerHTML;
    td.className = "thumb-logo";
    td.innerHTML = "<img src='" + "logos/thumb/" + fixName(venue) + ".png'>";
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

function addScrollActions() {
  if (! phoneGap || device.platform == "iOS")
    return;
  removeScrollActions();
  $(window).on("touchmove", function() {
    $.colorbox.close();
    return true;
  });
  $(window).on("scroll", function() {
    $.colorbox.close();
    return true;
  });
}

function removeScrollActions() {
  $(window).off("touchmove");
  $(window).off("scroll");
}

function setHardWidths() {
  if (device.platform != "iOS")
    return;
  var width = $(window).width() - 100;
  $("tr").each(function(key, node) {
    $(node).children("td").first().css({
      minWidth: width + "px",
      maxWidth: width + "px",
      textOverflow: "hidden",
      overflow: "hidden"
    });
  });
}

var limitedDisplay = false;

function miscMenu() {
  var sortString = "Sort By Scan Time";
  if (sortOrder == "scan")
    sortString = "Sort By Date";
  var restoreString = "";
  if (limitedDisplay)
    restoreString = "<a href='#' id='restore'>Restore Events</a>";
  var goingString = "";
  $.map(getSettings("shows"), function(id) {
    if ($("#event-" + id).length)
      goingString = "<a href='#' id='going'>Display Events I'm Going To</a>";
  });
  colorbox("<a href='#' id='show-venues'>Choose Venues to Exclude</a><a href='#' id='list-new'>List New Events</a><a href='#' id='export-calendar'>Export Calendar</a><a href='#' id='sort-method'>" +
	   sortString +
	   "</a><a href='#' id='choose-date'>Choose Date</a><a href='#' id='search'>Search</a>" +
	   restoreString +
	   goingString +
	   "<a href='#' id='about'>About</a><a href='#' id='csid-close'>Close</a>");
  $("#show-venues").bind("click", function() {
    showVenueChooser();
    return false;
  });
  $("#about").bind("click", function() {
    $.colorbox.close();
    var url = "http://lars.ingebrigtsen.no/2013/09/22/crowdsourcing-is-dead/";
    if (phoneGap && device.platform == "iOS")
      window.open(url, "_system", "location=no");
    else
      document.location.href = url;
    return false;
  });
  $("#export-calendar").hide();
  $("#export-calendar").bind("click", function() {
    $.colorbox.close();
    var shows = getSettings("shows");
    if (shows.length < 1)
      alert("No events to export");
    else 
      exportCalendar();
    return false;
  });
  $("#sort-method").bind("click", function() {
    $.colorbox.close();
    if (sortOrder == "date") {
      sortOrder = "scan";
      sortByScanOrder();
    } else {
      sortOrder = "date";
      restoreTable();
    }
    return false;
  });
  $("#choose-date").bind("click", function() {
    restoreTable();
    chooseDate();
    return false;
  });
  $("#search").bind("click", function() {
    restoreTable();
    searchEvents();
    return false;
  });
  $("#going").bind("click", function() {
    $.colorbox.close();
    limitedDisplay = true;
    hideShow(false, false, false, getSettings("shows"));
    return false;
  });
  $("#list-new").bind("click", function() {
    restoreTable();
    limitedDisplay = true;
    var blankTable = hideShow(false, getSettings("timestamp"));
    if (blankTable) {
      // Restore table.
      hideShow();
      colorbox("<a href='#' id='csid-close'>No new events have arrived since " +
	       getSettings("timestamp") + "</a>");
    } else {
      $.colorbox.close();
      var maxTimestamp = "";
      $("tr").each(function(key, node) {
	var timestamp = node.getAttribute("time");
	if (timestamp > maxTimestamp)
	  maxTimestamp = timestamp;
      });
      setSettings("timestamp", maxTimestamp);
    }
    return false;
  });
  $("#restore").bind("click", function() {
    restoreTable();
    $.colorbox.close();
    limitedDisplay = false;
    return false;
  });
  var func = function() {
    $("table").show();
    $.colorbox.close();
    $(".pika-single").remove();
    return false;
  };
  $("#csid-close").bind("click", func);
  $("#cboxLoadedContent").bind("click", func);
  addScrollActions();
}

function searchEvents() {
  colorbox("<div class='search' id='search-wrap'><form id='search-form'><input type='string' size=30 id='search-input'></form></div><a href='#' id='do-search'>Search</a><a href='#' id='csid-close'>Close</a>");
  $("#search-wrap").bind("click", function() {
    return false;
  });
  $("#search-input").focus();
  var func = function() {
    var match = $("#search-input").val();
    var blankTable = hideShow(false, false, match);
    if (blankTable) {
      // Restore table.
      hideShow();
      colorbox("<a href='#' id='csid-close'>No events matched the search string</a>");
    } else {
      $.colorbox.close();
      limitedDisplay = true;
    }
    return false;
  };
  $("#search-form").bind("submit", func);
  $("#do-search").bind("click", func);
}

function colorbox(html) {
  $.colorbox({html: html,
	      width: $(window).width() + "px",
	      closeButton: false,
	      transition: "none",
	      height: "100%",
	      className: "event-lightbox"});
  $("#csid-close").bind("click", function() {
    $.colorbox.close();
    return false;
  });
  $("#cboxLoadedContent").bind("click", function() {
    $.colorbox.close();
    return false;
  });
}

function chooseDate() {
  $("table").hide();
  var picker = new Pikaday({
    format: 'YYYY-MM-DD',
    onSelect: function(date) {
      picker._d.setHours(5);
      var iso = picker._d.toISOString().substring(0, 10);
      $("table").show();
      $(".pika-single").remove();
      var first = false;
      //var tr = $("tr[date=" + iso + "]")[0];
      $("tr").each(function(key, node) {
	var dat = node.getAttribute("date");
	if (dat && dat == iso && ! first)
	  first = node;
      });
      if (! first)
	colorbox("<a href='#' id='csid-close'>No events on this date</a>");
      else {
	$.colorbox.close();
	$('html, body').animate({
          scrollTop: $(first).prev().offset().top
	}, 2000);
      }
    }
  });
  document.body.appendChild(picker.el);
  return false;
}

function exportEvent(id) {
  var node = document.getElementById("event-" + id);
  var date = node.getAttribute("date").split("-");
  var venue = node.getAttribute("name");
  var url = $(node).find("a").attr("href");
  var title = $(node).find("a")[0].innerHTML;

  var startDate = new Date(date[0], date[1] - 1, date[2], 19, 00, 0, 0, 0);
  var endDate = new Date(date[0], date[1] - 1, date[2], 20, 00, 0, 0, 0);

  var success = function(message) {
  };
  var error = function(message) {
    alert("Unable to export event: " + message);
  };

  // create a calendar (iOS only for now)
  //window.plugins.calendar.createCalendar(calendarName,success,error);

  // create an event silently (on Android < 4 an interactive dialog is shown)
  window.plugins.calendar.createEvent(title, venue, "",
				      startDate, endDate, success, error);
}

function restoreTable() {
  var parent = $("table")[0].parentNode;
  $("table").remove();
  parent.appendChild(savedTable.clone({withDataAndEvents: true})[0]);
  if (! phoneGap)
    loadLogos(true);
}

function shareEvent(id) {
  var node = document.getElementById("event-" + id);
  var date = node.getAttribute("date").split("-");
  var venue = node.getAttribute("name");
  var url = $(node).find("a").attr("href");
  var title = $(node).find("a")[0].innerHTML;

  window.plugins.socialsharing.share
  ("I'm going to " + title + " at " + venue + " on " +
   new Date(date[0], date[1] - 1, date[2], 19, 00, 0, 0, 0).toDateString() +
   ".",
   title,
   //"http://csid.no/logos/larger/" + fixName(venue) + ".png",
   null,
   url);
}

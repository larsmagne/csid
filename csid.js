var reveal = false;
var phoneGap = false;
var sortOrder = "date";
var savedTable = false;
var sentrum = [59.915430, 10.751862];
var homePos = sentrum;
var autoSummaries = true;

var mapKey = "AIzaSyDOzwQi0pHvnJ1hW__DTC2H4f2qPCr3pWw";

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

  $("#selector").append("<div id='summaries' class='export " + visible + 
			"'><a class='summaries'>" + sumText() + "</a></div>");
  $("a.summaries").bind("click", function(e) {
    autoSummaries = ! autoSummaries;
    viewable();
    if (! autoSummaries) {
      setSettings("summariesOff", "yes");
      hideAllSummaries();
    } else
      setSettings("summariesOff", "no");
    this.innerHTML = sumText();
  });

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

  $("#selector").append("<div class='export'><a id='rss' href='csid.atom'>Atom/\RSS feed</a><p><a href='https://itunes.apple.com/us/app/csid-concerts-in-oslo/id1037896784?mt=8&ign-mpt=uo%3D4'><img src='assets/apple.png'></a><p><a href='https://play.google.com/store/apps/details?id=no.ingebrigtsen.csid'><img src='assets/google.png'></a><p><a href='https://www.microsoft.com/en-us/store/apps/concerts-in-oslo/9nblggh6c4lv'><img src='assets/windows.png'></a></div></div>");

  $("img#logo").bind("click", function() {
    window.location.href = "http://csid.no/";
  });

  $('a#help').bind("click", function() {
    $.ajax({
      url: "help.html",
      dataType: "text",
      success: function(data) {
	colorbox("<div class='help'>" + data + "</div>" +
		 "<a href='#' id='csid-close'>Close</a>");
      }
    });
    return false;
  });
    
  $('#small-menu').bind("click", function(e) {
    miscMenu();
    return false;
  });

  if (mobilep) {
    if (phoneGap && device.platform != "Win32NT")
      addLogos();
    else
      loadLogos(mobilep);
    $(window).on("orientationchange", function() {
      closeColorbox();
      if (phoneGap)
	setHardWidths();
      return true;
    });
    if (phoneGap) {
      setHardWidths();
      StatusBar.overlaysWebView(false);
    }
  } else {
    addDesktopLogos();
  }

  viewable();
  $("tr.date").click(function() {
    if (hasSummaries(this))
      hideSummaries(this);
    else
      showSummaries(this);
  });
  $(window).scroll(function() {
    clearTimeout($.data(this, 'scrollTimer'));
    $.data(this, 'scrollTimer', setTimeout(function() {
      viewable();
    }, 250));
  });
  addHoverSummaries();
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
    if (phoneGap || $("body").width() < 600)
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

var doneGotoShow = false;

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
      visible = (timestamp > onlyAfterTimestamp &&
		 $.inArray(name, venues) != -1);
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

  var gotoShow = window.location.href.match("goto=([0-9]+)");
  if (! doneGotoShow && gotoShow) {
    var id = gotoShow[1];
    var elem = $("#event-" + id);
    var shows = getSettings("shows");
    toggleShow(id, true);
    $("#show-" + id).attr('checked', true);
    $('body').animate({
      scrollTop: elem.position().top + "px"
    }, 2000);
    doneGotoShow = true;
  }

  viewable();

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

function sortByDistance() {
  navigator.geolocation.getCurrentPosition(function(pos) {
    homePos = [pos.coords.latitude, pos.coords.longitude];
    sortByDistanceCont();
  }, function() {
    // On failure to get the position, just center somewhere.
    colorbox("Unable to get the current location.");
  });
}

function sortByDistanceCont() {
  var today = new Date().toISOString().substring(0, 10);
  var trs = [];
  var count = 0;
  $("tr").map(function (index, elem) {
    var dat = this.getAttribute("date");
    if (dat && dat == today) {
      trs[count++] = elem;
      $(elem).removeClass("invisible");
    } else {
      $(elem).addClass("invisible");
      return;
    }
    var venue = elem.getAttribute("name");
    if (! venue || ! elem.getAttribute("lat"))
      return;
    var dist = distance(homePos,
			[elem.getAttribute("lat"), elem.getAttribute("lng")]);
    var d = document.createElement("div");
    d.className = "distance";
    if (dist < 10) {
      var meter = Math.ceil(dist * 1000);
      if (meter < 1)
	d.innerHTML = "you are there";
      else
	d.innerHTML = "" + meter + " meters";
    } else
      d.innerHTML = "" + Math.round((dist * 10) / 10) + " km";
      
    elem.childNodes[1].appendChild(d);
  });

  trs = trs.sort(function(a, b) {
    return distance(homePos, [b.getAttribute("lat"), b.getAttribute("lng")]) -
      distance(homePos, [a.getAttribute("lat"), a.getAttribute("lng")]);
  });
  var first = $("tr")[0];
  trs.forEach(function(elem) {
    var parent = elem.parentNode;
    insertAfter(elem, first);
  });
}

function insertAfter(newNode, referenceNode) {
  referenceNode.parentNode.insertBefore(newNode, referenceNode.nextSibling);
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

function eventMenu(id) {
  var elem = $("#" + id)[0];
  actionEventMenu(elem, elem.getAttribute("name"));
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
  var logo = "logos/larger/" + fixName(venue);
  if (phoneGap) {
    if (device.platform != "Win32NT")
      exportString = "<a href='#' id='export-event'>Export Event to Calendar</a>";
    exportString += "<a href='#' id='share-event'>Share Event</a>";
    if (! existingLogos[fixName(venue)])
      logo = "http://csid.no/logos/larger/" + fixName(venue);
  }
  colorbox("<div class='outer-venue-logo'><img src='" + logo +
	   ".png' srcset='" + logo +
	   "x2.png 2x'></div><div class='event-text'><div>" +
	   $(node).find("a")[0].innerHTML +
	   "</div></div><a id='event-link' href='" + link +
	   "'>Display the event web page</a><a href='#' id='mark-event'>" +
	   type + "</a>" + exportString +
	   "<a href='#' id='csid-close'>Close</a>");
  $("#mark-event").bind("click", function() {
    toggleShow(id, $.inArray(id, shows) == -1);
    closeColorbox();
    return false;
  });
  $("#export-event").bind("click", function() {
    closeColorbox();
    exportEvent(id);
    return false;
  });
  $("#share-event").bind("click", function() {
    closeColorbox();
    shareEvent(id);
    return false;
  });
  $("#event-link").bind("click", function() {
    closeColorbox();
    followLink(this.href);
    return false;
  });
}

function followLink(src) {
  if (phoneGap && device.platform != "Android")
    window.open(src, "_system", "location=no");
  else
    document.location.href = src;
}

function actionVenueMenu(name) {
  var displayName = name.replace(/_/g, " ");
  var limit = "Show events from " + displayName;
  if (lastVenue == name)
    limit = "Show all events again";
  var deniedVenues = getSettings("deniedVenues");
  var venues = "Exclude events from " + displayName;
  if ($.inArray(name, deniedVenues) != -1)
    venues = "Include events from " + displayName;

  var logo = "logos/larger/" + fixName(name);
  if (phoneGap) {
    if (! existingLogos[fixName(name)])
      logo = "http://csid.no/logos/larger/" + fixName(name);
  }
  colorbox("<div class='outer-venue-logo'><img src='" + logo +
	   ".png' srcset='" + logo +
	   "x2.png 2x'></div><a href='#' id='venue-limit'>" +
	   limit + "</a><a href='#' id='venue-mark'>" + venues +
	   "</a><a href='#' id='all-venues'>Show all events from all venues</a><a href='#' id='csid-close'>Close</a>");
  $("#venue-limit").bind("click", function() {
    if (lastVenue != name) {
      limitedDisplay = true;
      hideShow(name);
      lastVenue = name;
    } else {
      hideShow();
      lastVenue = false;
    }
    closeColorbox();
    return false;
  });

  $("#venue-mark").bind("click", function() {
    var deniedVenues = getSettings("deniedVenues");
    document.getElementById(name).checked =
      ($.inArray(name, deniedVenues) != -1);
    setVenueCookie();
    hideShow();
    closeColorbox();
    return false;
  });

  $("#all-venues").bind("click", function() {
    $("tr.invisible").each(function(key, node) {
      $(node).removeClass("invisible");
    });
    closeColorbox();
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
  venues += "<a href='#' id='csid-close'>Close</a>";
  colorbox(venues);
  box.style.position = "absolute";
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
    closeColorbox();
    return false;
  };
  $("div.venue-top").bind("click", func);
  $("#csid-close").bind("click", func);
}

function addLogos() {
  $("tr").each(function(key, node) {
    var venue = node.getAttribute("name");
    if (! venue)
      return;
    var td = node.childNodes[1];
    td.title = td.innerHTML;
    td.className = "thumb-logo";

    if (phoneGap && ! existingLogos[fixName(venue)]) {
      td.innerHTML = "<img src='http://csid.no/logos/thumb/" +
	fixName(venue) + ".png' srcset='http://csid.no/logos/thumb/" +
	fixName(venue) + "x2.png 2x'></td>";
    } else {
      td.innerHTML = "<img src='logos/thumb/" + fixName(venue) +
	".png' srcset='logos/thumb/" + fixName(venue) + "x2.png 2x'>";
    }
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

function addDesktopLogos() {
  $("tr").each(function(key, node) {
    var venue = node.getAttribute("name");
    if (! venue)
      return;
    var td = node.childNodes[1];
    var title = td.innerHTML;
    var focus = false;
    $(td).mouseenter(function() {
      var image = new Image();
      focus = true;
      image.onload = function() {
	if (focus) {
	  td.innerHTML = "";
	  td.appendChild(image);
	}
      };
      image.setAttribute("srcset", "logos/thumb/" + fixName(venue) + "x2.png 2x");
      image.src = "logos/thumb/" + fixName(venue) + ".png";
    });
    $(td).mouseleave(function() {
      focus = false;
      td.innerHTML = title;
    });
  });
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
  image.setAttribute("srcset", "logos/thumb/" + fixName(venue) + "x2.png 2x");
  image.src = "logos/thumb/" + fixName(venue) + ".png";
}

function hideDuplicates() {
  var seen = [];
  var shows = getSettings("shows");
  $("tr").each(function(key, node) {
    var id = node.id.replace(/^event-/, "");
    if (! id || id.match(/summary/))
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
  var summaryString = "<a href='#' id='menu-summaries'>Load Summaries</a>";
  if (autoSummaries)
    summaryString = "<a href='#' id='menu-summaries'>Don't Load Summaries</a>";
  var goingString = "";
  $.map(getSettings("shows"), function(id) {
    if ($("#event-" + id).length)
      goingString = "<a href='#' id='going'>Display Events I'm Going To</a>";
  });
  var pgString = "";
  var appString = "<div class='apps'><img src='assets/apple.png' id='apple'><img src='assets/google.png' id='google'><img src='assets/windows.png' id='windows'></div>";
  if (phoneGap) {
    pgString = "<a href='#' id='reload'>Reload Data</a>";
    appString = "";
  }
  colorbox("<a href='#' id='show-venues'>Choose Venues to Exclude</a><a href='#' id='show-map'>Show Today's Events on a Map</a>" +
	   (phoneGap? "<a href='#' id='list-closest'>List Today's Nearest Events</a>": "") +
	   "<a href='#' id='list-new'>List New Events</a><a href='#' id='export-calendar'>Export Calendar</a><a href='#' id='sort-method'>" +
	   sortString +
	   "</a><a href='#' id='choose-date'>Choose Date</a><a href='#' id='search'>Search</a>" +
	   summaryString +
	   restoreString +
	   goingString +
	   pgString +
	   "<a href='#' id='csid-close'>Close</a>" +
	   "<a href='#' id='add-venue'>Add Venue</a><a href='#' id='about'>About</a>" +
	   appString);
  $("#show-venues").bind("click", function() {
    showVenueChooser();
    return false;
  });
  var aboutPage = function() {
    closeColorbox();
    var url = "http://lars.ingebrigtsen.no/2013/09/22/crowdsourcing-is-dead/";
    followLink(url);
    return false;
  };
  $("#about").bind("click", aboutPage);
  $("#show-map").bind("click", showMap);
  $("#list-closest").bind("click", function() {
    limitedDisplay = true;
    sortByDistance();
  });
  $("#add-venue").bind("click", function() {
    colorbox("<a href='#' id='add'>To request a new venue, click here and leave a comment on the blog page.</a>");
    $("#add").bind("click", aboutPage);
    return false;
  });
  $("#apple").bind("click", function() {
    closeColorbox();
    document.location.href = "https://itunes.apple.com/us/app/csid-concerts-in-oslo/id1037896784?mt=8&ign-mpt=uo%3D4";
  });
  $("#google").bind("click", function() {
    closeColorbox();
    document.location.href = "https://play.google.com/store/apps/details?id=no.ingebrigtsen.csid";
  });
  $("#windows").bind("click", function() {
    closeColorbox();
    document.location.href = "https://www.microsoft.com/en-us/store/apps/concerts-in-oslo/9nblggh6c4lv";
  });
  $("#export-calendar").hide();
  $("#export-calendar").bind("click", function() {
    closeColorbox();
    var shows = getSettings("shows");
    if (shows.length < 1)
      alert("No events to export");
    else 
      exportCalendar();
    return false;
  });
  $("#sort-method").hide();
  $("#sort-method").bind("click", function() {
    closeColorbox();
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
  $("#reload").bind("click", function() {
    closeColorbox();
    loadData();
    return false;
  });
  $("#search").bind("click", function() {
    restoreTable();
    searchEvents();
    return false;
  });
  $("#going").bind("click", function() {
    closeColorbox();
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
      closeColorbox();
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
    closeColorbox();
    limitedDisplay = false;
    return false;
  });
  $("#menu-summaries").bind("click", function() {
    restoreTable();
    closeColorbox();
    autoSummaries = ! autoSummaries;
    viewable();
    if (! autoSummaries) {
      setSettings("summariesOff", "yes");
      hideAllSummaries();
    } else
      setSettings("summariesOff", "no");
    return false;
  });
  var func = function() {
    $("table").show();
    closeColorbox();
    $(".pika-single").remove();
    return false;
  };
  $("#csid-close").bind("click", func);
  $("#cboxLoadedContent").bind("click", func);
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
      closeColorbox();
      limitedDisplay = true;
    }
    return false;
  };
  $("#search-form").bind("submit", func);
  $("#do-search").bind("click", func);
}

var box = false;

function colorbox(html) {
  if (box) {
    $(box).remove();
    box = false;
  }
  box = document.createElement("div");
  box.style.position = "fixed";
  box.style.left = "0px";
  box.style.top = "0px";
  box.style.height = window.innerHeight + "px";
  box.style.width = window.innerWidth + "px";
  box.style.display = "block";
  box.style.background = "#105010";
  box.style.color = "black";
  box.style.padding = "0px";
  box.className = "event-lightbox";
  box.innerHTML = html;
  var func = function() {
    closeColorbox();
    return false;
  };
  $("#csid-close").bind("click", func);
  $(box).bind("click", function() {
    closeColorbox();
    return false;
  });
  document.body.appendChild(box);
}

function closeColorbox() {
  if (box) {
    $(box).remove();
    box = false;
  }
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
      $("tr").each(function(key, node) {
	var dat = node.getAttribute("date");
	if (dat && dat == iso && ! first)
	  first = node;
      });
      if (! first)
	colorbox("<a href='#' id='csid-close'>No events on this date</a>");
      else {
	closeColorbox();
	$('html, body').animate({
          scrollTop: $(first).prev().offset().top - 38
	}, 2000);
      }
    }
  });
  document.body.appendChild(picker.el);
  // Ensure that the calendar is visible if the page is scrolled.
  var box = $(".pika-single");
  box.style.position = "fixed";
  box.style.left = "0px";
  box.style.top = "0px";
  box.style.height = $(window).height() + "px";
  box.style.width = $(window).width() + "px";
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
  //window.plugins.calendar.createEvent(title, venue, "",
//				      startDate, endDate, success, error);
  window.plugins.calendar.createEventInteractively(title, venue, "",
						   startDate, endDate,
						   success,error);
}

function restoreTable() {
  $(".distance").remove();
  $("tr").removeClass("invisible");
  if (limitedDisplay) {
    hideShow();
    limitedDisplay = false;
  }
  return;
  /*
  var parent = $("table")[0].parentNode;
  $("table").remove();
  parent.appendChild(savedTable.clone({withDataAndEvents: true})[0]);
  if (! phoneGap)
    loadLogos(true);
  */
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

function allVenues() {
  var venues = [];
  var i = 0;
  $("input[type=checkbox]").each(function(key, node) {
    if (! node.id.match(/show/))
      venues[i++] = node.id;
  });
  return venues;
}

var startPos = homePos;
var herePos = false;

function showMap() {
  var box = document.createElement("div");
  box.style.position = "fixed";
  box.style.left = "0px";
  box.style.top = "0px";
  box.style.width = $(window).width() + "px";
  box.style.height = $(window).height() + "px";
  box.style.display = "block";
  box.style.background = "grey";
  box.style.padding = "0px";
  box.id = "box";
  var heading = document.createElement("div");
  heading.innerHTML = "<span id='hide-labels'>Hide Events</span><span id='show-labels'>Show Events</span><span id='close-map'>Close Map</span>";
  heading.className = "map-heading";
  var map = document.createElement("div");
  map.style.width = $(window).width() + "px";
  map.style.height = window.innerHeight - 30 + "px";
  map.id = "map";
  box.appendChild(map);
  box.appendChild(heading);
  document.body.appendChild(box);
  var func = function(e) {
    $(box).remove();
    e.preventDefault();
    return;
  };
  $('#close-map').click(func);
  $('#show-labels').click(showLabels);
  $('#hide-labels').click(hideLabels);
  showMapPos();  
}

function showMapPos() {
  if (phoneGap) 
    navigator.geolocation.getCurrentPosition(function(pos) {
      homePos = [pos.coords.latitude, pos.coords.longitude];
      showMapCont(homePos, homePos);
    }, function() {
      // On failure to get the position, just center somewhere.
      showMapCont(homePos, false);
    });
  else
    showMapCont(homePos, false);
}

function showMapCont(sp, hp) {
  startPos = sp;
  herePos = hp;
  var script = document.createElement("script");
  script.setAttribute("src", "https://maps.googleapis.com/maps/api/js?key=" +
		      mapKey + "&callback=initMap");
  document.body.appendChild(script);
}

function initMap() {
  google.maps.Marker.prototype.setLabel = function(label) {
    this.label = new MarkerLabel({
      map: this.map,
      marker: this,
      text: label
    });
    this.label.bindTo('position', this, 'position');
  };

  var MarkerLabel = function(options) {
    this.setValues(options);
    this.span = document.createElement('span');
    this.span.className = 'map-marker-label';
    $(this.span).click(function() {
      if (options.marker.eventId)
	eventMenu(options.marker.eventId);
    });
  };

  MarkerLabel.prototype = $.extend(new google.maps.OverlayView(), {
    onAdd: function() {
      this.getPanes().overlayImage.appendChild(this.span);
      var self = this;
      this.listeners = [
        google.maps.event.addListener(this, 'position_changed', function() {
	  self.draw();
	})];
    },
    draw: function() {
      var text = String(this.get('text'));
      var position = this.getProjection().fromLatLngToDivPixel(this.get('position'));
      this.span.innerHTML = text;
      this.span.style.left = (position.x - 50) + 'px';
      this.span.style.top = position.y + 'px';
    }
  });
  
  var map = new google.maps.Map(document.getElementById('map'), {
    zoom: 14,
    center: {lat: sentrum[0], lng: sentrum[1]}
  });
  var markerImage = new google.maps.MarkerImage('cross.png',
						new google.maps.Size(20, 20),
						new google.maps.Point(0, 0),
						new google.maps.Point(10, 10));
  var homeImage = new google.maps.MarkerImage('home-cross.png',
					      new google.maps.Size(20, 20),
					      new google.maps.Point(0, 0),
					      new google.maps.Point(10, 10));
  var bounds = new google.maps.LatLngBounds();
  var pos = collectPositions();
  if (herePos)
    pos["here"] = ["You are here", false, herePos[0], herePos[1]];
  for (var key in pos) {
    var venue = pos[key];
    var marker = new google.maps.Marker({
      map: map,
      position: {lat: venue[2], lng: venue[3]},
      label: venue[0] + (venue[1]? "<span>" + venue[1] + "</span>": ""),
      eventId: venue[4],
      icon: (key == "here"? homeImage: markerImage),
      draggable: false
    });
    if (key == "here")
      var hereMarker = marker;
    // Certain venues outside Oslo are disregarded when computing the bounds.
    else if (! venue[5])
      bounds.extend(marker.getPosition());
    var cFunc = function(id) {
      return function() {
	eventMenu(id);
      };
    };
    marker.addListener('click', cFunc(venue[4]));
  };
  if (herePos && map.getBounds().contains(hereMarker.getPosition()))
    map.setCenter(herePos);
  else
    map.setCenter(startPos);
  // Rescale the map to display all the events.
  map.fitBounds(bounds);
}

function collectPositions() {
  var today = new Date().toISOString().substring(0, 10);
  var pos = [];
  $("tr").each(function(key, node) {
    var dat = node.getAttribute("date");
    if (dat && dat == today) {
      var venue = node.getAttribute("name");
      var event = node.childNodes[0].childNodes[0].innerHTML;
      if (node.getAttribute("lat")) {
	if (pos[venue])
	  pos[venue][0] += "<br>" + event;
	else {
	  pos[venue] = [event, venue.replace(/_/, " "),
			parseFloat(node.getAttribute("lat")),
			parseFloat(node.getAttribute("lng")),
			node.id,
			node.getAttribute("nobound")
		       ];
	}
      }
    }
  });
  return pos;
}

function getDistanceFromLatLonInKm(lat1,lon1,lat2,lon2) {
  var R = 6371; // Radius of the earth in km
  var dLat = deg2rad(lat2-lat1);  // deg2rad below
  var dLon = deg2rad(lon2-lon1); 
  var a = 
      Math.sin(dLat/2) * Math.sin(dLat/2) +
      Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) * 
      Math.sin(dLon/2) * Math.sin(dLon/2)
  ; 
  var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a)); 
  var d = R * c; // Distance in km
  return d;
}

function deg2rad(deg) {
  return deg * (Math.PI/180);
}

function distance(a, b) {
  return getDistanceFromLatLonInKm(a[0], a[1], b[0], b[1]);
}

function showLabels() {
  $(".map-marker-label").show();
}

function hideLabels() {
  $(".map-marker-label").hide();
}

var summaryQuery = false;

function fetchSummaries(ids, index, callback) {
  var hash = sha1(ids[index][1]);
  var url = "summaries";
  for (var i = 0; i < 4; i++) {
    url += "/" + hash.substring(i * 10, (i + 1) * 10);
  }
  url += "-data.json";
  summaryQuery = $.ajax({
    url: url,
    dataType: "text",
    beforeSend: function(xhr){
      if (xhr.overrideMimeType)
      {
	xhr.overrideMimeType("application/json");
      }
    },
    success: function(data) {
      var json = $.parseJSON(data);
      if (callback)
	callback();
      insertSummary(ids[index][0], ids[index][1], json);
      if (index + 1 < ids.length)
	fetchSummaries(ids, index + 1);
    },
    error: function(data) {
      if (data.statusText == "abort")
	return;
      if (index + 1 < ids.length)
	fetchSummaries(ids, index + 1);
    }
  });
}

function insertSummary(id, url, data) {
  var tr = document.getElementById(id);
  // If there's already a summary here, then do nothing.
  if ($(tr).find("div.summary")[0])
    return;
  var td = tr.firstChild;
  var div = document.createElement("div");
  div.className = "summary";
  if (data.image) {
    var image = document.createElement("img");
    image.src = data.image;
    if (image.width > window.innerWidth / 2)
      image.width = window.innerWidth / 2;
    div.appendChild(image);
  }
  var text = document.createElement("div");
  text.innerHTML = data.summary;
  div.style.top = $(tr).height() + "px";
  div.style.width = $(tr).width() + "px";
  var offset = $(tr).offset();
  td.style.position = "relative";
  td.style.borderBottom = "200px solid white";
  td.nextSibling.style.borderBottom = "200px solid white";
  td.nextSibling.nextSibling.style.borderBottom = "200px solid white";
  document.body.appendChild(div);

  div.appendChild(text);
  td.appendChild(div);
  $(td).click(function() {
    document.location = url;
  });
}

function hasSummaries(tr) {
  tr = tr.nextSibling;
  while (tr && ! $(tr).hasClass("date")) {
    if ($(tr).find("div.summary")[0])
      return true;
    tr = tr.nextSibling;
  }
  return false;
}

function hideSummaries(tr) {
  if (summaryQuery)
    summaryQuery.abort();
  tr = tr.nextSibling;
  while (tr && ! $(tr).hasClass("date")) {
    if ($(tr).find("div.summary")) {
      var td = tr.firstChild;
      td.style.borderBottom = "0px";
      td.nextSibling.style.borderBottom = "0px";
      td.nextSibling.nextSibling.style.borderBottom = "0px";
      $(tr).find("div.summary").remove();
    }
    tr = tr.nextSibling;
  }
}

var fetchedSummaries = [];

function showSummaries(tr, noRepeat) {
  var ids = [];
  tr = tr.nextSibling;
  while (tr && ! $(tr).hasClass("date")) {
    var id = tr.getAttribute("id");
    if (id || id.match("event")) {
      var link = tr.firstChild.firstChild;
      if (! noRepeat || ! fetchedSummaries[link.href]) {
	ids.push([id, link.href]);
	fetchedSummaries[link.href] = true;
      }
    }
    tr = tr.nextSibling;
  }
  if (ids.length > 0)
    fetchSummaries(ids, 0);
}

function isVisible(node) {
  var offset = $(node).offset();
  var windowTop = $(window).scrollTop();
  return offset.top > windowTop &&
    offset.top < windowTop + window.innerHeight * 2;
}

function viewable() {
  if (! autoSummaries)
    return;
  var ids = [];
  $("tr").each(function(i, tr) {
    var id = tr.getAttribute("id");
    if (id &&
	id.match("event") &&
	isVisible(tr) &&
	! $(tr).find("div.summary")[0]) {
      var link = tr.firstChild.firstChild;
      if (! fetchedSummaries[link.href]) {
	ids.push([id, link.href]);
	fetchedSummaries[link.href] = true;
      }
    }
  });
  if (ids.length > 0)
    fetchSummaries(ids, 0);
}

function sumText() {
  if (getSettings("summariesOff") == "yes")
    autoSummaries = false;
  var sumText = "Summaries are loaded automatically";
  if (! autoSummaries)
    sumText = "Summaries are displayed when hovering";
  return sumText;
}

function addHoverSummaries() {
  $("tr").each(function(key, node) {
    var hover;
    var id = node.getAttribute("id");
    if (! id || ! id.match("event"))
      return;
    $(node).hover(function() {
      if (autoSummaries)
	return;
      var link = node.firstChild.firstChild.href;
      var ids = [];
      ids.push([id, link]);
      fetchSummaries(ids, 0);
    });
  });
}

function hideAllSummaries() {
  $("tr.date").each(function(key, node) {
    if (hasSummaries(node))
      hideSummaries(node);
  });
  fetchedSummaries = [];
}

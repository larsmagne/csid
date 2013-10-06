var reveal = false;

function getSettings(name) {
  var settings = $.cookie(name);
  var data = [];
  if (settings)
    data = settings.split(",");
  return data;
}

function addNavigation() {
  var venues = getSettings("venues");
  var shows = getSettings("shows");

  var added = 0;
  $("tr").each(function(key, node) {
    var name = node.getAttribute("name");
    if (! name)
      return;

    if (! $("input#" + name)[0]) {
      var checked = "";
      if (venues.length == 0 || venues.indexOf(name) != -1)
	checked = "checked";
      $("#selector").append("<label class='venue'><input type=checkbox " + 
			    checked + " id='" + name + "'>" +
			    name.replace("_", " ") + "</label> ");
      $("#" + name).bind("click", function(e) {
	hideShow();
      });
    }

    $(node.childNodes[1]).bind("click", function(e) {
      $("input[type=checkbox]").each(function(key, node) {
	if (node.id != name && ! node.id.match(/show/))
	  node.checked = reveal;
      });
      reveal = ! reveal;
      hideShow();
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
  hideShow();
}

function hideShow() {
  var venues = [];
  var i = 0;
  $("input[type=checkbox]").each(function(key, node) {
    if (node.checked && ! node.id.match(/show/))
      venues[i++] = node.id;
  });
  $.cookie("venues", venues.join(), { expires: 10000 });
  var prevDate = "";
  $("tr").each(function(key, node) {
    var name = node.getAttribute("name");
    if (! name)
      return;
    if (venues.indexOf(name) != -1) {
      $(node).removeClass("hidden");
      // Make just a single date field per day visible.
      $(node).find("div").each(function(key, date) {
	var text = date.innerHTML;
	if (text != prevDate)
	  $(date).removeClass("invisible");
	else 
	  $(date).addClass("invisible");
	prevDate = text;
      });
    } else
      $(node).addClass("hidden");
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

addNavigation();

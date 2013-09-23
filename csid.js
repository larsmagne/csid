function addNavigation() {
  var settings = $.cookie("venues");
  var venues = [];
  if (settings)
    venues = settings.split(",");
  var added = 0;
  $("tr").each(function(key, node) {
    var name = node.getAttribute("name");
    if (! $("input#" + name)[0]) {
      var checked = "";
      if (! settings || venues.indexOf(name) != -1)
	checked = "checked";
      $("#selector").append("<label class='venue'><input type=checkbox " + 
			    checked + " id='" + name + "'>" +
			    name + "</label>");
      if ((++added % 5) == 0)
	$("#selector").append("<br>");
      $("#" + name).bind("click", function(e) {
	hideShow();
      });
    }
  });
  hideShow();
}

function hideShow() {
  var venues = [];
  var i = 0;
  $("input[type=checkbox]").each(function(key, node) {
    if (node.checked)
      venues[i++] = node.id;
  });
  $.cookie("venues", venues.join());
  var prevDate = "";
  $("tr").each(function(key, node) {
    if (venues.indexOf(node.getAttribute("name")) != -1) {
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

addNavigation();

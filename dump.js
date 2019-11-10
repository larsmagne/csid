var page = new WebPage();
var fs = require('fs');

page.onLoadFinished = function() {
  console.log("page load finished");
  // We don't exit on page loads, because there'll be a bunch of
  // redirects.  Instead we wait for the timeout these days.
};

window.setTimeout(function () {
  console.log("timed out");
  fs.write('/tmp/event.html', page.content, 'w');
  phantom.exit();
}, 10000);

page.open(<URL>, function() {
  page.evaluate(function() {
  });
});

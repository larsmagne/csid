var page = new WebPage();
var fs = require('fs');

page.onLoadFinished = function() {
  console.log("page load finished");
  fs.write('/tmp/event.html', page.content, 'w');
  phantom.exit();
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

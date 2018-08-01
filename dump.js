var page = new WebPage();
var fs = require('fs');

page.onLoadFinished = function() {
  console.log("page load finished");
  fs.write('/tmp/event.html', page.content, 'w');
  phantom.exit();
};

page.open(<URL>, function() {
  page.evaluate(function() {
  });
});

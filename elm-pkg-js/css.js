exports.init = function init(_app) {
  console.log("Injecting stylesheet link");
  var rel = document.createAttribute("rel");
  rel.value = "stylesheet";
  var href = document.createAttribute("href");
  href.value = "/dogroll.css";
  var type = document.createAttribute("type");
  type.value = "text/css";
  var link = document.createElement("link");
  link.setAttributeNode(rel);
  link.setAttributeNode(href);
  link.setAttributeNode(type);
  document.head.appendChild(link);
};

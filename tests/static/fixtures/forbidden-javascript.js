eval("window.compromised = true");
const injected = new Function("return document.cookie");
setTimeout("window.compromised = true", 10);

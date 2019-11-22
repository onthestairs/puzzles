var fs = require("fs");
var camel = require("to-camel-case");
var api = require("../../api/swagger.json");
Object.keys(api.paths).forEach(function (path) {
    var parts = path.split("/");
    var joined = parts.join("-");
    var pathCamel = camel(joined);
    if ("get" in api.paths[path]) {
        api.paths[path].get.operationId = "get" + pathCamel;
        api.paths[path].get.produces = ["application/json"];
    }
    if ("post" in api.paths[path]) {
        api.paths[path].post.operationId = "post" + pathCamel;
        api.paths[path].post.produces = ["application/json"];
        api.paths[path].post.consumes = ["application/json"];
    }
});
console.log("writing file");
fs.writeFile("../api/api.json", JSON.stringify(api), function (err) {
    if (err) {
        console.error(err);
        return;
    }
    console.log("written");
});

const fs = require("fs");
const camel = require("to-camel-case");
const api = require("../../api/swagger.json");

Object.keys(api.paths).forEach((path: string) => {
  const parts = path.split("/");
  const joined = parts.join("-");
  const pathCamel = camel(joined);
  if ("get" in api.paths[path]) {
    api.paths[path].get.operationId = `get${pathCamel}`;
    api.paths[path].get.produces = ["application/json"];
  }
  if ("post" in api.paths[path]) {
    api.paths[path].post.operationId = `post${pathCamel}`;
    api.paths[path].post.produces = ["application/json"];
    api.paths[path].post.consumes = ["application/json"];
  }
});

console.log("writing file");
fs.writeFile("../api/api.json", JSON.stringify(api), err => {
  if (err) {
    console.error(err);
    return;
  }
  console.log("written");
});

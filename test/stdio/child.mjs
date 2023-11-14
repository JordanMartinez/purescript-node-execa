import process from "node:process";

console.log("Script started");
process.stdin.once("data", (str) => {
  console.log(`Got data from stdin: ${str}`);
});

const ms = 100;

setTimeout(() => {
  console.log("log 1");
  setTimeout(() => {
    console.error("error 1");
    setTimeout(() => {
      console.log("log 2");
      setTimeout(() => {
        console.error("error 2");
        console.log("Script terminated");
      }, ms);
    }, ms);
  }, ms);
}, ms);

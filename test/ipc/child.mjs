import process from "node:process";

console.log("Script started");

const tc = (msg, f) => {
  try {
    f();
    console.log(`${msg}: Success`);
  } catch (e) {
    console.log(`${msg}: Failure - ${e}`);
  }
};

process.on("message", (msg) => {
  console.log(`Child got parent message: ${msg}`);
});

tc("Child -> Parent Message", () => {
  process.send("Hello from child");
});

setTimeout(() => {
  tc("Child disconnect from parent", () => {
    process.disconnect();
  });
  
  console.log("Script finished");  
}, 100);

import child_process from "node:child_process";

const stdio = [ "ignore", "inherit", "inherit", "ipc"];

const tc = (msg, f) => {
  try {
    f();
    console.log(`${msg}: Success`);
  } catch (e) {
    console.log(`${msg}: Failure - ${e}`);
  }
};

const go = (idx) => {
  var child;
  if (idx === 1) {
    tc("\n\nSpawn - IPC usage", () => {
      child = child_process.spawn("node", ["child.mjs"], { stdio });
    });
  } else if (idx === 2) {
    tc("\n\nSpawnSync - IPC usage", () => {
      child = child_process.spawnSync("node", ["child.mjs"], { stdio });
    });
  } else if (idx === 3) {
    tc("\n\nFork - IPC usage", () => {
      child = child_process.fork("child.mjs", { stdio });
    });
  }
  if (!child) {
    if (idx !== 4) {
      go(idx+1);
    } else {
      console.log("Finished");
    }
  } else {
    tc("Parent receives message", () => {
      child.on("message", (msg) => {
        console.log(`Parent received message: ${msg}`);
      });
    });
    child.on("spawn", () => {
      tc("Parent -> Child", () => {
        child.send("Hello from Parent");
      });
    });
    child.on("exit", () => {
      go(idx + 1);
    });  
  }
};

go(1);

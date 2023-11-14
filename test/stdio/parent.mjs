import child_process from "node:child_process";

const tc = (succ, f) => {
  try {
    f();
    console.log(`${succ}: Success`);
  } catch (error) {
    console.log(`${succ}: Error - ${error}`);
  }
};

const canWrite = (streamName, stream) => {
  tc(`  can write to ${streamName}`, () => {
    stream.write("some data", "utf8", (err) => {
      if (err) {
        console.log(`  ${streamName} write error: ${err}`);
      }
    });
    stream.end();
  });
};

const canRead = (streamName, stream) => {
  tc(`  can read from ${streamName}`, () => {
    stream.on("data", (str) => {
      console.log(`  ${streamName}: ${str}`);
    });
  });
};

const printStatus = (testName, cp) => {
  console.log(`${testName}`)
  
  canWrite("stdin", cp.stdin)
  canRead("stdout", cp.stdout);
  canRead("stderr", cp.stderr);
}

const doSpawn = (opts, next) => {
  const testName = `[stdin: ${opts.stdin} | stdout: ${opts.stdout} | stderr: ${opts.stderr}]`;
  const cp = child_process.spawn("node", ["child.mjs"], {
    stdio: [ opts.stdin, opts.stdout, opts.stderr ]
  });
  cp.once("spawn", () => {
    cp.once("close", () => {
      console.log(`..Exited\n\n`);
      next();
    });
    printStatus(testName, cp);
    if (opts.stdin === "inherit") {
      console.log("This run won't exit until you press Ctrl+D to end stdin stream...\n");
    }
  });
};

const choices = [ "inherit", "ignore", "pipe" ]
const options = [];
choices.forEach(stdin => {
  choices.forEach(stdout => {
    choices.forEach(stderr => {
      options.push({stdin, stdout, stderr});
    });
  });
});

const go = (idx) => () => {
  if (idx == (3 * 3 * 3)) {
    console.log("Done");
  } else {
    doSpawn(options[idx], go(idx + 1));
  }
};
go(0)();

import cp from "child_process";
import process from "process";

const args = process.argv.slice(2);
console.log(`Script CLI args: ${args}`);

if (args[0] === "build") {
  cp.spawnSync("spago", ["build", "--purs-args", "--censor-lib --strict"], { stdio: "inherit"});
}
if (args[0] === "test") {
  cp.spawnSync("spago", ["-x", "test.dhall", "test"], { stdio: "inherit"});
}

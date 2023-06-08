import cp from "child_process";
import process from "process";

const args = process.argv.slice(2);
console.log(`Script CLI args: ${args}`);

if (args[0] === "build") {
  const result = cp.spawnSync("spago", ["build", "--purs-args", "--censor-lib --strict"], { stdio: "inherit"});
  console.log(result);
}
if (args[0] === "test") {
  const result = cp.spawnSync("spago", ["-x", "test.dhall", "test"], { stdio: "inherit"});
  console.log(result);
}

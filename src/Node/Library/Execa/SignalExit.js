import * as process from "node:process";

export function unsafeProcessHasProp(prop) {
  return global.process[prop] !== null && global.process[prop] !== undefined;
}

export function unsafeReadProcessProp(prop) {
  return global.process[prop];
}

export function unsafeWriteProcessProp(prop, value) {
  global.process[prop] = value;
}

export function processCallFn(originalProcessReallyExit, exitCode) {
  return originalProcessReallyExit.call(process, exitCode);
}

export function processKill(pid, sig) {
  process.kill(pid, sig);
}

export function processListenersLength(sig) {
  return process.listeners(sig).length;
}

export function processOn(sig, listener) {
  return process.on(sig, listener);
}

export function processOff(sig, listener) {
  return process.off(sig, listener);
}

export function customProcessEmit(cb) {
  return function (ev, arg) {
    const thisArg = this;
    const argumentsArg = arguments;
    return cb((originalProcessEmit) => originalProcessEmit.apply(thisArg, argumentsArg), ev, arg);
  };
}

export function processExitCode() {
  return process.exitCode;
}

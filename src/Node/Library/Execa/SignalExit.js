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
  return originalProcessReallyExit.call(global.process, exitCode);
}

export function customProcessEmit(cb) {
  return function (ev, arg) {
    const thisArg = this;
    const argumentsArg = arguments;
    return cb((originalProcessEmit) => originalProcessEmit.apply(thisArg, argumentsArg), ev, arg);
  };
}


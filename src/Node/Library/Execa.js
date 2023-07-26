export function monkeyPatchKill(cp, killFn) {
  cp.kill = killFn.bind(null, cp.kill.bind(cp))
}

// For execa-specifically.
export function setTimeoutImpl(timeout, cb) {
  const t = setTimeout(cb, timeout);
  // Guarded because there's no `.unref()` when `execa` is used in the renderer
	// process in Electron. This cannot be tested since we don't run tests in
	// Electron.
  return t.unref ? t : { unref: () => {} };
}

export function killImpl(cp, signal, options) {
  return cp.kill(signal, options);
}

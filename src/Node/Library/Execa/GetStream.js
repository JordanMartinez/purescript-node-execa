import stream from "node:stream";

export function pipeline(source, destination, cb) {
  return stream.pipeline([source, destination], cb);
}

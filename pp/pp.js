
const fs = require('fs');
const path = require('path')

const preprocess = (text, flags) => {
  const lines = text.split('\n')
  const result = []
  let current = []
  let any = []
  lines.forEach((line, index) => {
    // console.log(current, line)
    let match = line.match(/^#if\s+([A-Za-z0-9_-]+)\s*$/)
    if (match) {
      const value = !!flags[match[1]];
      current.push(value);
      any.push(value);
      return
    }
    match = line.match(/^#ifn\s+([A-Za-z0-9_-]+)\s*$/)
    if (match) {
      const value = !flags[match[1]];
      current.push(value);
      any.push(value);
      return
    }
    match = line.match(/^#endif\s*$/)
    if (match) {
      // console.log('#endif')
      if (!current.length) {
        throw new Error("dangling #endif on line " + (index + 1))
      }
      current.pop();
      any.pop();
      return
    }
    match = line.match(/^#else/)
    if (match) {
      if (!current.length) {
        throw new Error("dangling #else on line " + (index + 1))
      }
      const value = !current[current.length - 1];
      // when value is true, only replace #else if none have been satisfied
      if (value) {
        current[current.length - 1] = value && !any.some(x => x)
      } else {
        current[current.length - 1] = value
      }
      return
    }
    match = line.match(/^#elif\s+([A-Za-z0-9_-]+)\s*$/)
    if (match) {
      if (!current.length) {
        throw new Error("dangling #elif on line " + (index + 1))
      }
      const value = !!flags[match[1]];
      if (value) {
        any[any.length - 1] = value;
      }
      current[current.length - 1] = value;
      // console.log(flags, match[1], current)
      return
    } else if (current.every(flag => flag)) {
      result.push(line)
    }
  })
  return result.join('\n')
}

const flags = {}
const files = []
const dest = process.argv[2]
process.argv.slice(3).forEach(arg => {
  if (arg[0] === '-') {
    flags[arg.slice(1)] = true
  } else {
    files.push(arg)
  }
})

console.log("Dest", dest)
if (!fs.existsSync(dest)) {
  fs.mkdirSync(dest)
}
files.forEach(name => {
  const out = path.join(dest, path.basename(name))
  try {
    fs.writeFileSync(out, preprocess(fs.readFileSync(name).toString('utf8'), flags))
  } catch (error) {
    console.error(name)
    console.error(error.message)
    process.exit(1)
  }
})

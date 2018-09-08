
const fs = require('fs');
const path = require('path')

const preprocess = (text, flags) => {
  const lines = text.split('\n')
  const result = []
  let current = null
  lines.forEach(line => {
    let match = line.match(/^#if\s+([A-Za-z]+)\s*$/)
    if (match) {
      if (current !== null) {
        throw new Error("Cannot nest ifs")
      }
      current = !!flags[match[0]]
    } else {
      match = line.match(/^#endif/)
      if (match) {
        if (current === null) {
          throw new Error("dangling #endif")
        }
        current = null
      } else {
        match = line.match(/^#else/)
        if (match) {
          if (current === null) {
            throw new Error("dangling #else")
          }
          current = !current
        } else {
          match = line.match(/^#elif\s+([A-Za-z]+)\s*$/)
          if (match) {
            current = !!flags[match[0]]
          } else if (current === null || current === true) {
            result.push(line)
          }
        }
      }
    }
  })
  return result.join('\n')
}

const flags = []
const files = []
const dest = process.argv[2]
process.argv.slice(3).forEach(arg => {
  if (arg[0] === '-') {
    flags.push(arg.slice(1))
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
  fs.writeFileSync(out, preprocess(fs.readFileSync(name).toString('utf8'), flags))
})
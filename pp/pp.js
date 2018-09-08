
const fs = require('fs');
const path = require('path')

const preprocess = (text, flags) => {
  const lines = text.split('\n')
  const result = []
  let current = null
  lines.forEach(line => {
    // console.log(current, line)
    let match = line.match(/^#if\s+([A-Za-z0-9_-]+)\s*$/)
    if (match) {
      // console.log('match')
      if (current !== null) {
        throw new Error("Cannot nest ifs")
      }
      current = !!flags[match[1]]
            // console.log(flags, match[1], current)
    } else {
      match = line.match(/^#endif\s*$/)
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
          match = line.match(/^#elif\s+([A-Za-z0-9_-]+)\s*$/)
          if (match) {
            current = !!flags[match[1]]
            // console.log(flags, match[1], current)
          } else if (current === null || current === true) {
            result.push(line)
          }
        }
      }
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
  fs.writeFileSync(out, preprocess(fs.readFileSync(name).toString('utf8'), flags))
})
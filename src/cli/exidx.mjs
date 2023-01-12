// Sigma16: exidx.mjs
// Build html index of directory of asm.txt examples

// Copyright (C) 2023 John T. O'Donnell.  License: GNU GPL Version 3 or later
// See Sigma16/README, LICENSE, and https://jtod.github.io/home/Sigma16

// This file is part of Sigma16.  Sigma16 is free software: you can
// redistribute it and/or modify it under the terms of the GNU General
// Public License as published by the Free Software Foundation, either
// version 3 of the License, or (at your option) any later version.
// Sigma16 is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.  You should have received
// a copy of the GNU General Public License along with Sigma16.  If
// not, see <https://www.gnu.org/licenses/>.

// Usage: node .../exidx.mjs builds Index.html in examples directory

import * as fs from 'fs';

const asmSrcFile = /^\w+\.asm\.txt$/  // find asm source files
const exampleDir = /(Examples\S*)/    // find last part of directory path
const exampleName = /[^\:]*\:(.*)/    // find comment after : in first line
const leadingsemi = /\;(.*)/           // find leading semicolon in second line

// Find title of the index page
const dir = process.cwd ()            // current directory
const xs = dir.match(exampleDir)      // Examples/Arch/Group
let title = xs[0] ? xs[0] : "Examples"
title = title.replace(/\\/g, '/')     // replace \ by / in path

// Read directory
const flist = fs.readdirSync('.')     // all the files in directory

// Generate html
let htmlIdx = "<html>\n"
htmlIdx += "<head>\n"
htmlIdx +=
    "<style type=text/css media=screen>\n"
    + "body { font-family: sans-serif; }\n"
    + "h1 { font-size: 1.8em; }\n"
    + "a { font-weight: bold; font-size: 1.2em; }\n"
    + "p { margin-left: 5em; margin-bottom: 1em; }\n"
    + "ul { line-height: 1em; }\n"
    + "</style>\n"
htmlIdx += "</head>\n"
htmlIdx += "<body>\n"
let count = 0

htmlIdx += `<h1>${title}</h1>\n`

htmlIdx += '<ul>\n'

for (let f of flist) {
    if (asmSrcFile.test(f)) { // consider only *.asm.txt
        count++
        let text = fs.readFileSync (f)
        let xs = String(text).split ('\n')
        htmlIdx += `<li> <a href=\"./${f}\">${f}</a> `
        let rawline1 = xs[0]
        let ys = rawline1.match(exampleName)
        console.log (`ys = ${ys}`)
        let line1comment = ys[1]
        let rawline2 = xs[1]
        let zs = rawline2.match(leadingsemi)
        console.log (`zs = ${zs}`)
        let line2comment = zs[1]
        htmlIdx += "<p>" + line1comment + '\n' + line2comment + '\n' + "</p>\n"
        htmlIdx += '</li>\n'
    }
}

htmlIdx += '</ul>\n'
htmlIdx += '</body>\n</html>\n'
fs.writeFileSync ('Index.html', htmlIdx)
console.log (`Found ${count} assembly source files`)

        //        htmlIdx += '<pre>\n'
//        htmlIdx += '</pre>\n'
        //        htmlIdx +=
        //        htmlIdx += "</p>"
//        htmlIdx += xs[0] + '\n' + xs[1] + '\n'
// console.log (`dir = ${dir}`)
// console.log (exampleDir.test(dir))
// console.log (`xs = ${xs}`)
// console.log (`title = ${title}`)

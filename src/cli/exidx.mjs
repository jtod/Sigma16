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

// Usage: must run in Sigma16 source directory. node .../exidx.mjs
// builds index.html in examples directory and all its subdirectories.
// It assumes every source file ends in .asm.txt, and it uses the
// first two lines of text to find the text to display in the index.

import * as fs from 'fs';

// Parsing
const asmSrcFile = /\.asm\.txt$/      // find asm source files
const exampleDir = /(Examples\S*)/   // find last part of directory path
const exampleName = /[^\:]*\:(.*)/   // find comment after : in first line
const leadingsemi = /\;(.*)/         // find leading semicolon in second line

// Global count variables
let nSourceFiles = 0
let nDirectories = 0

// Header of the generated html
function initHtml (title) {
    let initHtmlIdx = "<html>\n"
    initHtmlIdx += "<head>\n"
    initHtmlIdx +=
        "<style type=text/css media=screen>\n"
        + "body { font-family: sans-serif; }\n"
        + "h1 { font-size: 1.8em; }\n"
        + "a { font-weight: bold; font-size: 1.2em; }\n"
        + "p { margin-left: 5em; margin-bottom: 1em; }\n"
        + "ul li { padding: 0.5em 0em; }\n"
        + "ul { line-height: 1em; }\n"
        + "</style>\n"
    initHtmlIdx += "</head>\n"
    initHtmlIdx += "<body>\n"
    initHtmlIdx += `<h1>${title}</h1>\n`
    initHtmlIdx += '<ul>\n'
    return initHtmlIdx
}

// Utility function: display info about a file path
function showFileStats (path, statsObj) {
    const description = statsObj.isFile() ? 'file'
          : statsObj.isDirectory() ? 'dir'
          : 'err'
    console.log (`showFileStats: path ${path} is ${description}`)
}

// Indentation of depth d
function depth (d) {
    return '   '.repeat(d)
}

// This is the top level function; the main program calls it, and when
// a subdirectory is encountered handleDir is called recursively.
// d = depth, topdir = .../Sigma16, f = path from Sigma16 to current

function handleDir (d, topdir, parents, f) {
    nDirectories++
    let path = `${topdir}${parents}/${f}`
    let extparents = `${parents}/${f}`
    console.log (`${depth(d)}Directory ${parents}/${f}`)
    let title = f
    title = title.replace(/\\/g, '/')     // replace \ by / in path
    let html = initHtml (title)
    if (d > 0) {
        html += `<li> <a href=\"../index.html">Up to ${parents}</a> </li>\n`
    }
    let newHtml = `<li>`
        + ` <a href=\"./${title}/index.html\">${title}</a>`
        + `</li>\n`
    const flist = fs.readdirSync(path)     // all the files in directory
    let fileCount = 0
    let subdirCount = 0
    for (let h of flist) {
        let hpath = `${path}/${h}`
        const fStats = fs.statSync (hpath)
        if (fStats.isFile()) {
            fileCount++
            html += handleFile (d+1, topdir, extparents, h)
        } else if (fStats.isDirectory()) {
            subdirCount++
            html += handleDir (d+1, topdir, extparents, h)
        } else {
            console.log (`${depth(d)}bad path ${h}`)
        }
    }

    // Print summary and finish html
    console.log (`${depth(d+1)}Directory ${f}:`
                 + ` found ${fileCount} source files`
                 + ` and ${subdirCount} subdirectories`)
    html += '</ul>\n'
    html += '</body>\n</html>\n'
    const indexPath = `${path}/index.html`
    fs.writeFileSync (indexPath, html)
    return newHtml
}

function handleFile (d, topdir, parents, f) {
    let html = ''
    let path = `${topdir}${parents}/${f}`
//    console.log (`${depth(d)}handleFile par=${parents} f=${f}`)
    if (path.search(asmSrcFile) > -1) { // consider only *.asm.txt
        nSourceFiles++
        console.log (`${depth(d)}Source file ${f}`)
        let text = fs.readFileSync (path)
        let xs = String(text).split ('\n')
        html += `<li> <a href=\"./${f}\">${f}</a> `
        let rawline1 = xs[0]
        let ys = rawline1.match(exampleName)
        let line1comment = ys[1]
        let rawline2 = xs[1]
        let zs = rawline2.match(leadingsemi)
        let line2comment = zs[1]
        html += line1comment + '\n' + line2comment + '\n'
        html += '</li>\n'
    } else {
        console.log (`${depth(d)}Skipping ${f}`)
    }
    return html
}

// Main program: exidx should be launched from the Sigma16 source
// directory, and it starts with the Examples directory

const sigdir = process.cwd ()            // current directory
console.log (`exidx starting in ${sigdir}`)
handleDir (0,  sigdir, '', 'Examples')
console.log (`Found ${nDirectories} directories`
             + ` and ${nSourceFiles} source files.`)
console.log ('exidx finished')

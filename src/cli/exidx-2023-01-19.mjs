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
// bash alias: exidx

import * as fs from 'fs';

// Parsing
const asmSrcFile = /\.asm\.txt$/      // find asm source files
const exampleDir = /(Examples\S*)/   // find last part of directory path
const exampleName = /[^\:]*\:(.*)/   // find comment after : in first line
const leadingsemi = /\;(.*)/         // find leading semicolon in second line

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

function handleDir (d, topdir, f, g) {
    // Identify directory and find title of the index page
    let f2 =  f + '/' + g
    let dir = topdir + '/' + f2
//    let dir = topdir + '/' + f
    console.log (`${depth(d)}Directory ${f}`)
    console.log (`${depth(d)}Directory ${f2} dir=${dir}`)
//    console.log (`${depth(d)}dir = ${dir}`)
    let title = g
//    console.log (`${depth(d)}raw title = ${title}`)
    title = title.replace(/\\/g, '/')     // replace \ by / in path
//    console.log (`${depth(d)}title = ${title}`)
    let html = initHtml (title)
    //    let newHtml = `<li>${title}</li>\n`
    let newHtml = `<li> Directory`
        + ` <a href=\"./${title}\">${title}</a>`
        + `</li>\n`
    console.log (`${depth(d)}newHtml = ${newHtml}`)
//    if (d > 0) return newHtml
    
    const flist = fs.readdirSync(dir)     // all the files in directory
    let fileCount = 0
    let subdirCount = 0
    for (let h of flist) {
        let f3 =  f2 + '/' + h
        let path = dir + '/' + f3
        console.log (`${depth(d)} f3 = ${f3}`)
//      console.log (`${depth(d)}handle dir reading file ${path}`)
        const fStats = fs.statSync (path)
        if (fStats.isFile()) {
            fileCount++
            html += handleFile (d+1, topdir, f, g)
        } else if (fStats.isDirectory()) {
            subdirCount++
            html += handleDir (d+1, topdir, f2)
        } else {
            console.log (`${depth(d)}bad path ${f2}`)
        }
    }

    // Print summary and finish html
    console.log (`${depth(d+1)}Directory ${f}: found ${fileCount} source files`
                 + ` and ${subdirCount} subdirectories`)
    html += '</ul>\n'
    html += '</body>\n</html>\n'
    const idxFile = `./${f}/index.html`
    console.log (`${depth(d+1)}writing ${idxFile}`)
    fs.writeFileSync (idxFile, html)
    return newHtml
}

function handleFile (d, topdir, f, g) {
    let html = ''
    let f2 =  f + '/' + g
    let path = topdir + '/' + f2
//    console.log (`${depth(d)}handleFile f=${f} g=${g}`)
//    console.log (`${depth(d)}Look at path = ${path}`)
//    let foo = path.search(asmSrcFile)
//    console.log (`${depth(d)}search ${path} ${foo}`)
    if (path.search(asmSrcFile) > -1) { // consider only *.asm.txt
        console.log (`${depth(d)}Source file ${g}`)
//        console.log (`${depth(d)}path = ${path}`)
        let text = fs.readFileSync (path)
        let xs = String(text).split ('\n')
        html += `<li> Program <a href=\"./${g}\">${g}</a> `
        let rawline1 = xs[0]
        let ys = rawline1.match(exampleName)
//        console.log (`${depth(d)}ys = ${ys}`)
        let line1comment = ys[1]
        let rawline2 = xs[1]
        let zs = rawline2.match(leadingsemi)
//        console.log (`${depth(d)}zs = ${zs}`)
        let line2comment = zs[1]
        html += "<p>" + line1comment + '\n' + line2comment + '\n' + "</p>\n"
        html += '</li>\n'
    } else {
        console.log (`${depth(d)}Skipping ${f2}`)
    }
    return html
}

// Main program: exidx should be launched from the Sigma16 source
// directory, and it starts with the Examples directory

const sigdir = process.cwd ()            // current directory
console.log (`exidx starting in ${sigdir}`)
handleDir (0, sigdir, '.', 'Examples')
console.log ('exidx finished')

    //    const dir = process.cwd ()            // current directory
    //    console.log (`Handle directory ${f}, starting in ${topdir}`)
//    const xs = dir.match(exampleDir)      // Examples/Arch/Group
    //    let title = xs[0] ? xs[0] : "Examples"
//    const flist = fs.readdirSync('.')     // all the files in directory
//    const dirStats = fs.statSync ('.')
//    showFileStats ('.', dirStats)

// Read directory
//    console.log (`${depth(d)}reading dir = ${dir}`)
//    console.log (`${depth(d)}finished reading dir`)
//    const dirStats = fs.statSync (dir)
//    console.log (`${depth(d)}****** html = ${html}`)
//        count++
//        console.log (`${depth(d)}have fstats for ${count}`)
//        showFileStats (fpath, fStats)

//    console.log (`${depth(d)}****** html = ${html}`)

//    console.log (`${depth(d)}**********************`)
//    console.log (html)
//    console.log (`${depth(d)}**********************`)
//    console.log (`${depth(d)}newHtml = ${newHtml}`)

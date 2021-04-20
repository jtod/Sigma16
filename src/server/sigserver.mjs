// sigserver.mjs
// Copyright (C) 2021 John T. O'Donnell
// email: john.t.odonnell9@gmail.com
// License: GNU GPL Version 3 or later. See Sigma16/README.md, LICENSE.txt

// server.mjs is a web server for Sigma16 using node.js and express

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

//-----------------------------------------------------------------------------
// Configuration
//-----------------------------------------------------------------------------

const THISRELEASE = '3.2.4'     // version to be served by main launch link
const LATEST_RELEASE = "3.2.2"  // report this when queried for status/latest
const LOCALPORT = 3000          // default port to use on local machine

//-----------------------------------------------------------------------------
// Usage
//-----------------------------------------------------------------------------

// This program can be used as a local server for testing and also as
// a web server on Heroku.

// To run the server locally:
//   In Sigma16 directory:  node src/server/sigserver.mjs
//   Using bash alias:      sigma16

//-----------------------------------------------------------------------------
// Packages
//-----------------------------------------------------------------------------

import express from 'express';
import * as http from 'http'
import * as path from 'path'
import * as cors from 'cors'
import * as ejs from 'ejs'
import * as fs from "fs";
import { fileURLToPath } from 'url';

//-----------------------------------------------------------------------------
// Files
//-----------------------------------------------------------------------------

// Find paths to components of the software, relative to this file.
// Assume the server is running in a directory that contains build/.
// The constant serverHome contains the path to that directory.

// From the Sigma16 directory, the path to the server is
// src/server/sigserver.mjg.  The server finds the home directory as
// serverpath/../.. and names it ServerHome.

// The directory structure needs to be the same in the Sigma16 folder
// and the server folder for the file paths to work in both.

const ServerHome = path.dirname (fileURLToPath (import.meta.url));
const S16home = path.join (ServerHome, '../..')
const CurrentReleaseDir = path.join (S16home,
                                     `build/Sigma16/releases/${THISRELEASE}`)

const LaunchFile = path.join (CurrentReleaseDir, 'Sigma16.html')
const DocsDir     = path.join (CurrentReleaseDir, "docs");
const BaseDir = path.join (CurrentReleaseDir, "src/base");
const GuiDir = path.join (CurrentReleaseDir, "src/gui");
const ExamplesDir = path.join (CurrentReleaseDir, "examples");

console.log ('Starting server')
console.log (`S16home = ${S16home}`)
console.log (`LaunchFile = ${LaunchFile}`)
console.log (`DocsDir = ${DocsDir}`)

// console.log (`CurrentReleaseDir = ${CurrentReleaseDir}`)
// console.log (`BaseDir = ${BaseDir}`)
// console.log (`GuiDir = ${GuiDir}`)
// console.log (`ExamplesDir = ${ExamplesDir}`)

//----------------------------------------------------------------------------
// Server
//----------------------------------------------------------------------------

const PORT = process.env.PORT || LOCALPORT

// This program can run locally in a shell and launch the GUI in a
// browser.  It communicates with the browser using a port, which
// should be between 1024 and 49151.  The default is defined to be
// 3000; this can be changed to avoid clash with any other
// application.

// Run express

const app = express();

app.set ('view engine', 'ejs')

//----------------------------------------------------------------------------
// Provide latest version on request
//----------------------------------------------------------------------------

// URL:  SERVER/status/latest/i.j.k

// Launching Sigma16. When Sigma16 initializes, it makes an http fetch
// to /status/latest/i.j.k, where i.j.k identifies the running
// version.  The server logs the request and responds with a string
// giving the latest release.

app.get ('/status/latest/*', (req,res) => {
    console.log ('status/latest has fired')
    const reqInfo = {
        date: new Date (),
        ip: req.ip,
        path: req.path
    }
    const xs = JSON.stringify (reqInfo)
    console.log (`Sigma16 request ${xs}`)
    res.type ('text/plain')
    const reply = LATEST_RELEASE
    res.send (reply)
})

//----------------------------------------------------------------------------
// Respond to request to launch Sigma16
//----------------------------------------------------------------------------

// URL:  SERVER/Sigma16/Sigma16.html

// Deliver mime types
express.static.mime.define({'application/javascript': ['js']});
express.static.mime.define({'text/css': ['css']});
express.static.mime.define({'text/html': ['html']});

// Serve static files from directories
app.use ('/Sigma16/src/base', express.static (BaseDir))
app.use ('/Sigma16/src/gui',  express.static (GuiDir))
app.use ('/Sigma16/docs',     express.static (DocsDir))
app.use ('/Sigma16/examples', express.static (ExamplesDir))

// Without the res.set statements for Cross-Origin, Chrome gives the
// deprecation warning: shared memory requires cross origin isolation

// With those statements, the examples aren't shown, but the welcome
// page and user guide do appear

app.get('/Sigma16/Sigma16.html', (req, res) => {
    console.log ('app.get Sigma16.html has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (LaunchFile)
})


app.get('/Sigma16/docs/*', (req, res) => {
    console.log ('app.get generic docs rule has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (DocsDir)
})

app.get('/Sigma16/examples/index.html', (req, res) => {
    console.log ('app.get index.html has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (path.join (ExamplesDir, 'index.html'))
})

app.get('/Sigma16/examples/*', (req, res) => {
    console.log ('app.get generic examples rule has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (ExamplesDir)
})

app.get('/Sigma16/src/base/*', (req, res) => {
    console.log ('app.get Sigma16/src/base/* has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (BaseDir)
})

app.get('/Sigma16/src/gui/*', (req, res) => {
    console.log ('app.get Sigma16/src/gui/* has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (GuiDir)
})

// emwt and its imports need special handling

app.get('/Sigma16/emwt.mjs', (req, res) => {
    console.log ('app.get src/base/emwt.mjs has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (`${CurrentReleaseDir}/src/base/emwt.mjs`)
})

app.get('/Sigma16/common.mjs', (req, res) => {
    console.log ('app.get common.mjs has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (`${CurrentReleaseDir}/src/base/common.mjs`)
})

app.get('/Sigma16/arithmetic.mjs', (req, res) => {
    console.log ('app.get arithmetic.mjs has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (`${CurrentReleaseDir}/src/base/arithmetic.mjs`)
})
app.get('/Sigma16/state.mjs', (req, res) => {
    console.log ('app.get state.mjs has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (`${CurrentReleaseDir}/src/base/state.mjs`)
})
app.get('/Sigma16/emulator.mjs', (req, res) => {
    console.log ('app.get emulator.mjs has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (`${CurrentReleaseDir}/src/base/emulator.mjs`)
})
app.get('/Sigma16/s16module.mjs', (req, res) => {
    console.log ('app.get s16module.mjs has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (`${CurrentReleaseDir}/src/base/s16module.mjs`)
})
app.get('/Sigma16/architecture.mjs', (req, res) => {
    console.log ('app.get architectur.mjs has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (`${CurrentReleaseDir}/src/base/architecture.mjs`)
})
app.get('/Sigma16/assembler.mjs', (req, res) => {
    console.log ('app.get assembler.mjs has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (`${CurrentReleaseDir}/src/base/assembler.mjs`)
})
app.get('/Sigma16/linker.mjs', (req, res) => {
    console.log ('app.get linker.mjs has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (`${CurrentReleaseDir}/src/base/linker.mjs`)
})

//----------------------------------------------------------------------------
// Notes: cross origin isolation
//----------------------------------------------------------------------------

// Miscellaneous
// app.use (cors ())
// app.use (express.static ('public'))

// https://developer.chrome.com/blog/enabling-shared-array-buffer/

// SharedArrayBuffer is currently available in Desktop Chrome, but
// from Chrome 91 it will be limited to cross-origin isolated pages.
// You can make a page cross-origin isolated by serving the page with
// these headers:

// Cross-Origin-Embedder-Policy: require-corp
// Cross-Origin-Opener-Policy: same-origin

// Once you do this, your page will not be able to load cross-origin
// content unless the resource explicitly allows it via a
// Cross-Origin-Resource-Policy header or CORS headers
// (Access-Control-Allow-* and so forth).

//----------------------------------------------------------------------------
// Testing
//----------------------------------------------------------------------------

// Test url requests
app.get ('/hello.html', (req,res) => {
    res.render ('hello')
})

app.get ('/world.html', (req,res) => {
    res.render ('world')
})

//----------------------------------------------------------------------------
// Main program: start server
//----------------------------------------------------------------------------

console.log ('SigServer main')
app.listen(PORT, () => console.log(`Listening on port ${PORT}`));

//----------------------------------------------------------------------------
// Deprecated
//----------------------------------------------------------------------------

/*

// For consistency with Heroku server, the main program is not inside
// a function.

// export function main () {
// }

const localPort = 3000;

// Using sendFile for static resource.  For dynamic page, use res.render
app.get ('/', (req,res) => {
    res.sendFile (path.resolve (__dirname, 'pages/index.html'))
})

// Start the server
export function launchGUI () {
    app.listen(localPort, () => console.log(`Listening on port ${localPort}`));
    console.log (`Open http://localhost:${localPort}/ in your browser`);
}

// from first version of SigServer providing latest version
// app.get ('/', (req,res) => {
//     res.render ('index')
// })

//-----------------------------------------------------------------------------
// Launch gui in browser (visit http://localhost:<port>/)
//-----------------------------------------------------------------------------

// this worked, not needed
app.get('/Sigma16/docs/Welcome/welcome.html', (req, res) => {
    console.log ('app.get generic  Welcome docs/* rule has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (DocsDir + '/Welcome/welcome.html')
})

// DOCS

app.get('/Sigma16/docs/UserGuide/Sigma16UserGuide.html', (req, res) => {
    console.log ('app.get UG rule has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (UG)
})
// app.get('/docs/*', (req, res) => res.sendFile(DocsDir));

app.get('/Sigma16/src/base/architecture.mjs', (req, res) => {
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (`${BaseDir}/architecture.mjs`)
})
*/

// Provide locations of static source files, docs, and examples
/*
app.use( '/', express.static (guiDir));
app.use( '/home', express.static (homeDir));
app.use( '/gui', express.static (guiDir));
app.use( '/base', express.static (baseDir));
app.use( '/datafiles', express.static (datafilesDir));
app.use( '/docs', express.static (docsDir));
app.use( '/examples', express.static (examplesDir));

// Deliver mime types as required by CORS
express.static.mime.define({'application/javascript': ['js']});
express.static.mime.define({'text/css': ['css']});

// Respond to get requests by sending the requested files
app.get('/', (req, res) => {
    res.set ('foobar', 'baz')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile(homeDir + '/Sigma16.html')
})

app.get('/gui/*', (req, res) => {
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile(guiDir)
})

app.get('/base/*', (req, res) => {
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile(baseDir)
})

app.get('/docs/*', (req, res) => res.sendFile(docsDir));

app.get('*', (req, res) => {
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    //    res.sendFile(guiDir)
    res.sendFile(srcDir)
})
// const UG = path.join (CurrentReleaseDir, "/docs/UserGuide/Sigma16UserGuide.html");

// const cliDir = path.dirname (fileURLToPath (import.meta.url));
// const homeDir = path.join (cliDir, "/../../")

// app.use ('Sigma16', express.static (CurrentReleaseDir))
// app.get ('/status/latest/*', cors (), (req,res) => {

// console.log (`serverHome = ${serverHome}`)
// const homeDir = path.dirname (fileURLToPath (import.meta.url));
// const guiDir = path.join (homeDir, "/src/gui/")
// const compatDir = path.join (homeDir, "/src/compatibility/")
// const srcDir = path.join (serverHome, "/src/")
// console.log (`srcDir = ${srcDir}`)
// const baseDir = path.join (homeDir, "/src/base/");
// const docsDir = path.join (homeDir, "/docs/")
const examplesDir = path.join (homeDir, "/examples");
// const datafilesDir = path.join (homeDir, "/src/datafiles/");

// app.use ('/Sigma16/docs/UserGuide', express.static (UGDir))

app.get('/Sigma16/examples/index.html', (req, res) => {
    console.log ('app.get Specific examples/index.html rule has fired')
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (ExamplesDir + '/index.html')
})

*/

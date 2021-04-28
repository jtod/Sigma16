// sigserver.mjs
// Copyright (C) 2021 John T. O'Donnell
// email: john.t.odonnell9@gmail.com
// License: GNU GPL Version 3 or later. See Sigma16/README.md, LICENSE.txt

// sigserver.mjs is a web server for Sigma16 using node.js and express

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
// Usage
//-----------------------------------------------------------------------------

// This program is a web server for Sigma16.  It can run either on a
// local computer, or as an Internet web server on Heroku.

// To run the server locally:
//   In Sigma16 directory:  node src/server/sigserver.mjs
//   Using bash cli alias:  sigma16
// To access it via the local server:
//   http://localhost:3000/Sigma16/Sigma16.html
//   http://latest/status/this/is/logged

//-----------------------------------------------------------------------------
// URLs
//-----------------------------------------------------------------------------

// Index page with general information
//    https://sigma16.herokuapp.com/

// Run the latest release (main run link points here)
//    https://sigma16.herokuapp.com/Sigma16

// Run current development version
//    https://sigma16.herokuapp.com/Sigma16/development

// Request latest release number (from running version 1/2/3)
//    https://sigma16.herokuapp.com/status/latest/1/2/3
//    http://localhost:3000/status/latest/3.4.5

// Run a specific release, e.g. 3.3.1
//    https://sigma16.herokuapp.com/Sigma16/release/3.3.1

//-----------------------------------------------------------------------------
// Configuration
//-----------------------------------------------------------------------------

// Parameters are in .env.  S16_LATEST_RELEASE is the version number of
// the latest official release, and S16_DEV_VERSION is the version number
// of the current development folder.  The http port should be between
// 1024 and 49151.  The default is LOCAL_PORT, which can be changed to
// avoid clash with any other application.

//-----------------------------------------------------------------------------
// Environment variables
//-----------------------------------------------------------------------------

// On the local server, the environment variables are set in .bashrc.
// On Heroku, they are set using heroku config

// If the environment defines a port (e.g. on the Heroku server) that
// is used; otherwise the LOCAL_PORT is used.  From the Sigma16
// directory, the path to the server is src/server/sigserver.mjs.

// The server finds the home directory as serverpath/../.. and names
// it S16home.

// Environment parameters.  On local server, these are set in .bashrc.
// On Heroku server, they are set using heroku config.

const S16_LATEST_RELEASE = process.env.S16_LATEST_RELEASE
const S16_DEV_VERSION = process.env.S16_DEV_VERSION
const S16_RUN_ENV = process.env.S16_RUN_ENV
const S16_LOCAL_PORT = process.env.S16_LOCAL_PORT

// S16_LATEST_RELEASE is the version number of the latest official
// release.  This is the version that users should be running.  The
// status/latest request returns this value and it's displayed on the
// Options page; that enables the user to see whether they are running
// the latest release.

// Calculated parameters

const PORT = process.env.PORT || S16_LOCAL_PORT
const ServerHome = path.dirname (fileURLToPath (import.meta.url));
const S16home = path.join (ServerHome, '../..')
const LatestReleaseDir = path.join (
    S16home,
    `build/Sigma16/release/${S16_DEV_VERSION}`)

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
// Directory structure
//-----------------------------------------------------------------------------

// The directory structure needs to be the same in the Sigma16 folder
// and the server folder for the file paths to work in both.

// build
//   Sigma16
//     release
//       3.2.4
//         Sigma16
//           Sigma16.html
//           VERSION.txt
//           LATESTVERSION.txt
//           LICENSE.txt
//           src
//             gui
//             base
//             server
//               sigserver.mjs
//             (other files not needed by server)
//           docs
//             docstyle.css
//             welcome
//             UserGuide
//             help
//           examples
//             index.html
//             

//----------------------------------------------------------------------------
// Server
//----------------------------------------------------------------------------

const app = express();
app.set ('view engine', 'ejs')
express.static.mime.define({'application/javascript': ['js']});
express.static.mime.define({'text/css': ['css']});
express.static.mime.define({'text/html': ['html']});

//----------------------------------------------------------------------------
// Provide latest version on request
// URL path: status/latest/i.j.k
//----------------------------------------------------------------------------

// When Sigma16 initializes, it makes an http fetch to
// /status/latest/i.j.k, where i.j.k identifies the running version.
// The server logs the request and responds with a string giving the
// latest release.

// app.get ('/status/latest/*', (req,res) => {
app.get ('/status/latest/:callerversion', (req,res) => {
    console.log ('status/latest')
    console.log (req.params.callerversion)
    const reqInfo = {
        date: new Date (),
        ip: req.ip,
        path: req.path,
        callerversion: req.params.callerversion
    }
    const xs = JSON.stringify (reqInfo)
    console.log (`Sigma16 request ${xs}`)
    res.type ('text/plain')
    res.set ('Access-Control-Allow-Origin', '*')
    const reply = S16_LATEST_RELEASE
    res.send (reply)
})

//----------------------------------------------------------------------------
// Respond to request to launch Sigma16
// URL path: Sigma16/Sigma16.html
//----------------------------------------------------------------------------

app.get('/Sigma16', (req, res) => {
    let loc = path.join (LatestReleaseDir)
    console.log (`Run latest release`)
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (loc)
})

app.get('/Sigma16/*.txt', (req, res) => {
    let loc = path.join (LatestReleaseDir, req.path)
//    console.log (`get top txt ${req.path}`)
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (loc)
})

app.get('/Sigma16/src/*', (req, res) => {
    let loc = path.join (LatestReleaseDir, req.path)
//    console.log (`get src ${req.path}`)
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (loc)
})


app.get('/Sigma16/docs/docstyle.css', (req, res) => {
    let loc = path.join (LatestReleaseDir, req.path)
//    console.log (`get docstyle ${req.path}`)
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (loc)
})

app.get('/Sigma16/*.mjs', (req, res) => {
    let basename = path.basename (req.path)
    let loc = path.join (LatestReleaseDir, 'Sigma16/src/base', basename)
//    console.log (`get mjs path=${req.path}`)
//    console.log (`get mjs loc=${loc}`)
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (loc)
})

//----------------------------------------------------------------------------
// Notes: cross origin isolation
//----------------------------------------------------------------------------

// Miscellaneous
// app.use (cors ())
// app.use (express.static ('public'))

// Without the res.set statements for Cross-Origin, Chrome gives the
// deprecation warning (April 2021) because shared memory requires
// cross origin isolation.  It is expected that Chrome 91 (May 2021)
// will simply refuse to create the shared array.

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
// URL path: SERVER://hello.html
// URL path: world.html
//----------------------------------------------------------------------------

// Test url requests
app.get ('/hello.html', (req,res) => {
    res.render ('hello')
})

app.get ('/world.html', (req,res) => {
    res.render ('world')
})

//----------------------------------------------------------------------------
// Main program
//----------------------------------------------------------------------------

console.log ('Starting sigserver')
console.log ('Environment:')
console.log (`  S16_LATEST_RELEASE = ${S16_LATEST_RELEASE}`)
console.log (`  S16_DEV_VERSION = ${S16_DEV_VERSION}`)
console.log (`  S16_RUN_ENV = ${S16_RUN_ENV}`)
console.log (`  S16_LOCAL_PORT = ${S16_LOCAL_PORT}`)
console.log (`Using port ${PORT}`)
console.log (`S16home = ${S16home}`)
console.log (`LatestReleaseDir = ${LatestReleaseDir}`)

app.listen(PORT, () => console.log(`Listening on port ${PORT}`));

//----------------------------------------------------------------------------
// Notes
//----------------------------------------------------------------------------

// General advice on URLs: https://www.w3.org/Provider/Style/URI.html

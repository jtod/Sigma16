// sigserver.mjs
// Copyright (C) 2021 John T. O'Donnell
// email: john.t.odonnell9@gmail.com
// License: GNU GPL Version 3 or later. See Sigma16/README.md, LICENSE.txt

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
// SigServer
//-----------------------------------------------------------------------------

// Sigma16 normally runs in a browser, although it can also run as a
// standalone program without Internet access using either a command
// line interface or a GUI provided by electron.

// To run in a browser, Sigma16 needs to be served via https.  A
// static server (such as github.io) is insufficient, because Sigma16
// uses concurrent process with shared memory, and(from May 2021)
// browsers require cross origin isolation in order to use shared
// memory. This program is a web server that enforces cross origin
// isolation, so it works for Sigma16.

// This server can run on a local computer for offline testing, or on
// an Internet server for production use.  To run it locally, execute
// `node src/server/sigserver.mjs'.  The bash cli alias sigma16 also
// issues that command.  SigmaSystem/SigServer pushes the program to
// Heroku to run on the Internet.

// To use a local server, launch sigserver.mjs and enter this URL:
//    http://localhost:3000/...Sigma16 path...
// To use the Internet server, enter this:
//    https://sigma16.herokuapp.com/...Sigma16 path...

// The server supports the following URL paths:
//    Return version number of the latest release:
//       .../status/latest
//    Launch the latest release:
//       .../build/release/Sigma16/Sigma16.html
//    Launch the development version:
//       .../build/dev/Sigma16/Sigma16.html
//    Launch specific version, e.g. 3.3.1:
//       .../build/3.3.1/Sigma16/Sigma16.html
//    Index page with general information
//       .../

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
// Configuration
//-----------------------------------------------------------------------------

// On the local server, the environment variables are set in .bashrc.
// On Heroku, they are set using heroku config

// Versions

// S16_LATEST_RELEASE is reported as response to status/latest query
// S16_RELEASE_VERSION is substituted for 'release' in http path
// These are typically the same in production but different for testing
// S16_DEV_VERSION is substituted for 'dev' in http path

const S16_LATEST_RELEASE = process.env.S16_LATEST_RELEASE
const S16_RELEASE_VERSION = process.env.S16_RELEASE_VERSION
const S16_DEV_VERSION = process.env.S16_DEV_VERSION

// Server configuration

const S16_LOCAL_PORT = process.env.S16_LOCAL_PORT
const S16_RUN_ENV = process.env.S16_RUN_ENV
const S16_SERVER_DIR = path.dirname (fileURLToPath (import.meta.url))

// The build directory contains a directory for each version.  The
// directory name may be a version number 3.3.1 or release or dev.
// All versions are launched relative to the build directory.  This
// location depends on whether the server is running on a local
// development machine or the Heroku Internet server.

let S16_BUILD_DIR
if (S16_RUN_ENV === 'Heroku') {
    console.log ('Running on Heroku')
    // Find the directory this program is running in and use that to
    // find the build directory
    S16_BUILD_DIR = path.join (S16_SERVER_DIR, '..', '..', 'build')
} else if (S16_RUN_ENV === 'Local') {
    console.log ('Running on local development machine')
    S16_BUILD_DIR = process.env.S16_LOCAL_BUILD_DIR
} else {
    console.log (`Server error: cannot find build directory for ${S16_RUN_ENV}`)
}

// If the environment defines an http port (e.g. on the Heroku server)
// that is used; otherwise the default LOCAL_PORT is used.  The http
// port should be between 1024 and 49151.  LOCAL_PORT is defined in an
// environment variable, and can be changed to avoid clash with any
// other application.

const PORT = process.env.PORT || S16_LOCAL_PORT

//----------------------------------------------------------------------------
// Server
//----------------------------------------------------------------------------

const app = express();
app.set ('view engine', 'ejs')
express.static.mime.define({'application/javascript': ['js']});
express.static.mime.define({'text/css': ['css']});
express.static.mime.define({'text/html': ['html']});

//----------------------------------------------------------------------------
// Top index
//----------------------------------------------------------------------------

app.get ('/', (req,res) => {
    console.log (`responding to get /`)
    res.sendFile (path.join (S16_BUILD_DIR, 'index.html'))
})

//----------------------------------------------------------------------------
// Provide latest version on request
// URL path: status/latest/i.j.k
//----------------------------------------------------------------------------

// When Sigma16 initializes, it makes an http fetch to
// /status/latest/i.j.k, where i.j.k identifies the running version.
// The server logs the request and responds with a string giving the
// latest release; the value of the string is S16_LATEST_RELEASE.
// That value is displayed on the Options page, enabling the user to
// see whether they are running the latest release.

app.get ('/status/latest/:callerversion', (req,res) => {
    const reqInfo = {
        date: new Date (),
        ip: req.ip,
        path: req.path,
        callerversion: req.params.callerversion
    }
    const xs = JSON.stringify (reqInfo)
    console.log (`responding-status-latest ${xs}`)
    res.type ('text/plain')
    res.set ('Access-Control-Allow-Origin', '*')
    const reply = S16_LATEST_RELEASE
    res.send (reply)
})

//----------------------------------------------------------------------------
// Request to launch Sigma16
// URL path: build/:version/Sigma16/Sigma16.html
//----------------------------------------------------------------------------

// Calculate actual version number.  The http request may ask for a
// specific version (1.2.3) or a symbolically named version (release,
// dev).  A symbolically named version is substituted with the
// corresponding version number which is used to find the files.

function substituteVersion (v) {
    return v === 'release' ? S16_RELEASE_VERSION
        : v === 'dev' ? S16_DEV_VERSION
        : v
}

// Provide response headers and send the file

function finish (req, res, loc) {
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
    res.sendFile (loc)
}

app.get('/build/:version/Sigma16/Sigma16.html', (req, res) => {
    const raw_v = req.params.version
    const v = substituteVersion (raw_v)
    const loc = path.join (S16_BUILD_DIR, v, 'Sigma16', 'Sigma16.html')
    console.log (`launching ${raw_v}->${v}`)
    console.log (`launching at loc ${loc}`)
    finish (req, res, loc)
})

app.get('/build/:version/Sigma16/:a/:b/:c/*', (req, res) => {
    const v = substituteVersion (req.params.version)
    const loc = path.join (S16_BUILD_DIR, v, 'Sigma16',
                           req.params.a,
                           req.params.b,
                           req.params.c,
                           path.basename (req.path))
    finish (req, res, loc)
})

app.get('/build/:version/Sigma16/:a/:b/*', (req, res) => {
    const v = substituteVersion (req.params.version)
    const loc = path.join (S16_BUILD_DIR, v, 'Sigma16',
                           req.params.a,
                           req.params.b,
                           path.basename (req.path))
    finish (req, res, loc)
})

app.get('/build/:version/Sigma16/:a/*', (req, res) => {
    const v = substituteVersion (req.params.version)
    const loc = path.join (S16_BUILD_DIR, v, 'Sigma16',
                           req.params.a,
                           path.basename (req.path))
    finish (req, res, loc)
})

// There are no mjs files in the Sigma16 directory.  However, the base
// emulator files are loaded by emwt when the processor is entered,
// and they are accessed by URL paths in the Sigma16 directory (not in
// Sigma16/src/base).  This rule must come after the rules that match
// src/gui/* and src/base/*

app.get('/build/:version/Sigma16/*.mjs', (req, res) => {
    const v = substituteVersion (req.params.version)
    const loc = path.join (S16_BUILD_DIR, v, 'Sigma16',
                           'src', 'base', path.basename (req.path))
    finish (req, res, loc)
})

//----------------------------------------------------------------------------
// Cross origin isolation
//----------------------------------------------------------------------------

// Miscellaneous
// app.use (cors ())
// app.use (express.static ('public'))

// Without the res.set statements for Cross-Origin, Chrome gives a
// deprecation warning (April 2021) because shared memory requires
// cross origin isolation.  It is expected that Chrome 91 (May 2021)
// will simply refuse to create the shared array.

// https://developer.chrome.com/blog/enabling-shared-array-buffer/

// SharedArrayBuffer is currently available in Desktop Chrome, but
// from Chrome 91 it will be limited to cross-origin isolated pages.
// You can make a page cross-origin isolated by serving the page with
// these headers:

//   Cross-Origin-Embedder-Policy: require-corp
//   Cross-Origin-Opener-Policy: same-origin

// Once you do this, your page will not be able to load cross-origin
// content unless the resource explicitly allows it via a
// Cross-Origin-Resource-Policy header or CORS headers
// (Access-Control-Allow-* and so forth).

//----------------------------------------------------------------------------
// Testing
// URL path: hello.html
// URL path: world.html
//----------------------------------------------------------------------------

app.get ('/hello.html', (req,res) => {
    res.render ('hello')
})

app.get ('/world.html', (req,res) => {
    res.render ('world')
})

//----------------------------------------------------------------------------
// Main program
//----------------------------------------------------------------------------

console.log (`Starting sigserver`)
console.log (`S16_RUN_ENV = ${S16_RUN_ENV}`)
console.log (`S16_LATEST_RELEASE = ${S16_LATEST_RELEASE}`)
console.log (`S16_RELEASE_VERSION = ${S16_RELEASE_VERSION}`)
console.log (`S16_DEV_VERSION = ${S16_DEV_VERSION}`)
console.log (`S16_SERVER_DIR = ${S16_SERVER_DIR}`)
console.log (`S16_BUILD_DIR = ${S16_BUILD_DIR}`)
app.listen(PORT, () => console.log(`Server is listening on port ${PORT}`));

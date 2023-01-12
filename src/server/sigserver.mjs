// sigserver.mjs
// Copyright (C) 2023 John T. O'Donnell.  License: GNU GPL Version 3 or later
// See Sigma16/README, LICENSE, and https://jtod.github.io/home/Sigma16

// This file is part of Sigma16.  Sigma16 is free software: you can
// redistribute it and/or modify it under the terms of the GNU
// General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option)
// any later version.  Sigma16 is distributed in the hope that it
// will be useful, but WITHOUT ANY WARRANTY; without even the
// implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE.  See the GNU General Public License for more details.
// You should have received a copy of the GNU General Public License
// along with Sigma16.  If not, see <https://www.gnu.org/licenses/>.

//-----------------------------------------------------------------------
// Sigma Server
//-----------------------------------------------------------------------

// Sigma16 can run either in a browser which provides a graphical user
// interface (GUI), or in a shell which provides a text interface.

// Sigma16 provides a command line interface to some of its
// components, including the assembler and linker. In this case the
// interaction uses text commands in a shell, and is executed by
// node.js.  There is no GUI, a browser is not required, and this
// server program is not required.  For example: `sigma16 assemble
// myprog' will translate myprog.asm.txt to machine language
// myprog.obj.txt.

// The GUI runs in a browser, which requires Sigma16 to fetch its
// files from a server via https.  This program is the server, and it
// can run without modification either on an Internet host (Heroku) or
// on a local machine, using environment variables to adapt.

// Sigma16 won't run on github pages because github.io provides only a
// static server.  Sigma16 uses concurrent processes with shared
// memory, and (from May 2021) browsers require cross origin isolation
// in order to use shared memory.  Since github.io does not serve
// pages with the required headers, it doesn't support Sigma16.  In
// contrast, this program is a web server that enforces cross origin
// isolation, and it works for Sigma16 using a modern browser.  It can
// run on a local computer for offline testing, or on an Internet
// server for production use.

// To run the server on a local machine:
//    - execute `node src/server/runserver.mjs'
//    - visit http://localhost:3000/...(see below)...
// To use the Internet server:
//    - visit https://sigma16.herokuapp.com
//        /Sigma16/build/release/Sigma16/Sigma16.html

//-----------------------------------------------------------------------
// Directories and files
//-----------------------------------------------------------------------

// $(SIGMASYSTEM)/src/Sigma16/build/dev/Sigma16           development
// $(SIGMASYSTEM)/src/server/Sigma16/build/3.4.2/Sigma16  local repository
// /app/build/3.4.2/Sigma16                               Heroku server

// All executable versions are stored in a build directory, with a
// path of the form .../build/VERSION/Sigma16 where VERSION can be a
// specific version number (e.g. "3.4.2") or "release" or "dev".

// The server accesses the files in two steps:
//   1. When the server launches, it sets S16_BUILD_DIR to the
//      build directory.  This depends on S15_RUN_ENV, which indicates
//      whether the program is running locally or on the server,
//      as well as the command line arguments, which can specify the
//      version to run.
//   2. When a client makes a request, the URL cam specify release,
//      dev, or a specific version

//-----------------------------------------------------------------------
// URLs
//-----------------------------------------------------------------------

// The server supports the following URLs, where xxx is either
//    https://sigma16.herokuapp.com/
//    http://localhost:3000/

// Index page with general information
//    xxx/

// Return version number of the latest release of Sigma16, where
// i.j.k is the version number of the build making the request
//   xxx/sigma16/status/latest/i.j.k     used in newer versions
//   xxx/status/latest/i.j.k             used in older versions

// Launch current release on Heroku server
// The server replaces "release" with S16_RELEASE_VERSION
// https://sigma16.herokuapp.com/Sigma16/build/release/Sigma16/Sigma16.html

// Launch development test version on Heroku server.
// This would normally be a copy of the local dev version
// The server replaces "test" with S16_DEV_VERSION
// https://sigma16.herokuapp.com/Sigma16/build/release/Sigma16/Sigma16.html

// Launch specified release i.j.k on Heroku server
// https://sigma16.herokuapp.com/Sigma16/build/i.j.k/Sigma16/Sigma16.html
// Example: version 3.4.0
// https://sigma16.herokuapp.com/Sigma16/build/3.4.0/Sigma16/Sigma16.html

// Run development version on local machine
// The server replaces "dev" with S16_DEV_VERSION
// http://localhost:3000/sigma16/build/dev/Sigma16/Sigma16.html

// Run specified version i.j.k from server repository on local machine
// Launch server with: sigma16 version
// Then provide version number in URL, e.g. for version 3.2.4 use:
// http://localhost:3000/sigma16/build/3.2.4/Sigma16/Sigma16.html

// Testing the server
//   xxx/pseudopassword/hello.html
//   xxx/pseudopassword/world.html

// DEPRECATED
//    xxx/sigma16/run                       *** NEW Latest release
//    xxx/sigma16/dev                       *** NEW dev version
//    xxx/sigma16/test                      *** NEW release candidate
//    xxx/sigma16/version/VERSION           *** NEW launch specific version
//    xxx/build/release/Sigma16/Sigma16.html     *** OLD deprecated
//    xxx/build/dev/Sigma16/Sigma16.html         *** deprecated
//    xxx/build/3.4.0/Sigma16/Sigma16.html       *** deprecated

//-----------------------------------------------------------------------
// Translating URL to file
//-----------------------------------------------------------------------

// The Sigma16 Home Page uses the following URL for the "Click to
// launch Sigma16" link:

//   https://sigma16.herokuapp.com/Sigma16
//     /build/release/Sigma16/Sigma16.html

// The server substitutes the current release number for "release",
// and serves the following file:

//   /app/Sigma16/build/3.4.2/Sigma16/Sigma16.html

// When the top html file requests further URLs, it does so relative
// to build/release and the server translates them all to build/3.4.2
// (or whatever the current release version is).

//-----------------------------------------------------------------------
// Packages
//-----------------------------------------------------------------------

import express from 'express';
import * as http from 'http'
import * as path from 'path'
import * as cors from 'cors'
import * as ejs from 'ejs'
import * as fs from "fs";
import { fileURLToPath } from 'url';

//-----------------------------------------------------------------------
// Configuration
//-----------------------------------------------------------------------

// On the local server, the environment variables are set in .bashrc.
// On Heroku, they are set using heroku config

// Query
//   S16_LATEST_RELEASE is reported as response to status/latest query

// Release version
//   S16_RELEASE_VERSION is substituted for 'release' in http path
//   These are typically the same in production but different for testing

// Development version
//   S16_DEV_VERSION is substituted for 'dev' in http path

// To set the environment variables:
//   heroku login
//   heroku config:set FOOBAR=VALUE   -- set environment variable
//   heroku config                    -- show configuration variables

// Read and save the environment variables for versions
const S16_LATEST_RELEASE = process.env.S16_LATEST_RELEASE
const S16_RELEASE_VERSION = process.env.S16_RELEASE_VERSION
const S16_DEV_VERSION = process.env.S16_DEV_VERSION

// Environment variables: Server configuration
const S16_LOCAL_PORT = process.env.S16_LOCAL_PORT
const S16_RUN_ENV = process.env.S16_RUN_ENV
const SIGMASYSTEM = process.env.SIGMASYSTEM
const S16_SERVER_DIR = path.dirname (fileURLToPath (import.meta.url))

const SIGSERVER_REPOSITORY = `${SIGMASYSTEM}/server`
console.log (`SIGMASYSTEM = ${SIGMASYSTEM}`)
console.log (`SIGSERVER_REPOSITORY = ${SIGSERVER_REPOSITORY}`)

let S16_LOCAL_BUILD_DIR  // set by StartServer if running locally

// The build directory contains a directory for each version.  The
// directory name may be a version number 3.3.1 or dev.  All versions
// are launched relative to the build directory.  This location
// depends on whether the server is running on a local development
// machine or the Heroku Internet server.

let S16_BUILD_DIR

// If the environment defines an http port (e.g. on the Heroku server)
// that is used; otherwise the default S16_LOCAL_PORT is used.  The
// http port should be between 1024 and 49151.  S16_LOCAL_PORT is
// defined in an environment variable, and can be changed to avoid
// clash with any other application.

const PORT = process.env.PORT || S16_LOCAL_PORT

//-----------------------------------------------------------------------
// Server
//-----------------------------------------------------------------------

const app = express();
app.set ('view engine', 'ejs')
express.static.mime.define({'application/javascript': ['js']});
express.static.mime.define({'text/css': ['css']});
express.static.mime.define({'text/html': ['html']});

//-----------------------------------------------------------------------
// Top index
//-----------------------------------------------------------------------

app.get ('/', (req,res) => {
    console.log (`responding-/`)
    res.sendFile (path.join ('/app', 'topindex.html'))
})

app.get ('/index.html', (req,res) => {
    console.log (`responding-/-index.html`)
    res.sendFile (path.join ('/app', 'topindex.html'))
})

app.get ('/default.html', (req,res) => {
    console.log (`responding-/-default.html`)
    res.sendFile (path.join ('/app', 'topindex.html'))
})

app.get ('/docstyle.css', (req,res) => {
    console.log (`responding-/`)
    res.sendFile (path.join ('/app', 'docstyle.css'))
})

//-----------------------------------------------------------------------
// Provide latest version on request
// URL path: /sigma16/status/latest/i.j.k
//-----------------------------------------------------------------------

// When Sigma16 initializes, it makes an http fetch to
// /status/latest/i.j.k, where i.j.k identifies the running version.
// The server logs the request and responds with a string giving the
// latest release; the value of the string is S16_LATEST_RELEASE.
// That value is displayed on the Options page, enabling the user to
// see whether they are running the latest release.

// Older versions use this form; keep for backward compatibility
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

// Starting URL with /sigma16 allows for status request for future
// programs
app.get ('/sigma16/status/latest/:callerversion', (req,res) => {
    const reqInfo = {
        date: new Date (),
        ip: req.ip,
        path: req.path,
        callerversion: req.params.callerversion
    }
    const xs = JSON.stringify (reqInfo)
    console.log (`responding-sigma16-status-latest ${xs}`)
    res.type ('text/plain')
    res.set ('Access-Control-Allow-Origin', '*')
    const reply = S16_LATEST_RELEASE
    res.send (reply)
})

//-----------------------------------------------------------------------
// Request to launch Sigma16
// URL path: sigma16/build/:version/Sigma16/Sigma16.html
//-----------------------------------------------------------------------

// Calculate actual version number.  The http request may ask for a
// specific version (1.2.3) or a symbolically named version (release,
// dev).  A symbolically named version is substituted with the
// corresponding version number which is used to find the files.

function substituteVersion (v) {
    return v === 'release' ? S16_RELEASE_VERSION
        : v === 'test' ? S16_DEV_VERSION
        : v === 'dev' ? 'dev'
        : v
}

// Provide response headers and send the file

function finish (req, res, loc) {
    res.set ('Cross-Origin-Embedder-Policy', 'require-corp')
    res.set ('Cross-Origin-Opener-Policy', 'same-origin')
//    console.log (loc)
    res.sendFile (loc)
}

// start page

app.get('/sigma16/build/:version/Sigma16/Sigma16.html', (req, res) => {
    const raw_v = req.params.version
    const v = substituteVersion (raw_v)
    const loc = path.join (S16_BUILD_DIR, v, 'Sigma16', 'Sigma16.html')
    console.log (`launching ${raw_v}->${v} at location ${loc}`)
    finish (req, res, loc)
})

// emwt

app.get('/sigma16/build/:version/Sigma16/emwt.mjs', (req, res) => {
    const raw_v = req.params.version
    const v = substituteVersion (raw_v)
    const loc = path.join (S16_BUILD_DIR, v, 'Sigma16',
                           'src', 'base', 'emwt.mjs')
    console.log (`reading emwt ${raw_v}->${v} at location ${loc}`)
    finish (req, res, loc)
})

// emulator core

app.get('/sigma16/build/:version/Sigma16/emcore.wasm', (req, res) => {
    console.log ('responding-emcore.wasm')
    const raw_v = req.params.version
    const v = substituteVersion (raw_v)
    const loc = path.join (S16_BUILD_DIR, v, 'Sigma16',
                           'src', 'base', 'emcore.wasm')
    console.log (`responding-emcore ${raw_v}->${v} at location ${loc}`)
    res.set ('Access-Control-Allow-Origin', '*')
    finish (req, res, loc)
})

// generic file paths

app.get('/sigma16/build/:version/Sigma16/:a/:b/:c/*', (req, res) => {
    const v = substituteVersion (req.params.version)
    const loc = path.join (S16_BUILD_DIR, v, 'Sigma16',
                           req.params.a,
                           req.params.b,
                           req.params.c,
                           path.basename (req.path))
    finish (req, res, loc)
})

app.get('/sigma16/build/:version/Sigma16/:a/:b/*', (req, res) => {
    const v = substituteVersion (req.params.version)
    const loc = path.join (S16_BUILD_DIR, v, 'Sigma16',
                           req.params.a,
                           req.params.b,
                           path.basename (req.path))
    finish (req, res, loc)
})

app.get('/sigma16/build/:version/Sigma16/:a/*', (req, res) => {
    const v = substituteVersion (req.params.version)
    const loc = path.join (S16_BUILD_DIR, v, 'Sigma16',
                           req.params.a,
                           path.basename (req.path))
    finish (req, res, loc)
})

// There are no mjs files in the Sigma16 directory.  However, the base
// emulator files are loaded by emwt when the processor is entered,
// and they are accessed by URL paths in the Sigma16 directory (not in
// Sigma16/src/base).  They are provided by this rule, which must come
// after the rules that match src/gui/* and src/base/*

app.get('/sigma16/build/:version/Sigma16/*.mjs', (req, res) => {
    const v = substituteVersion (req.params.version)
    const loc = path.join (S16_BUILD_DIR, v, 'Sigma16',
                           'src', 'base', path.basename (req.path))
    finish (req, res, loc)
})

//-----------------------------------------------------------------------
// Cross origin isolation
//-----------------------------------------------------------------------

// app.use (cors ())
// app.use (express.static ('public'))

// Without the res.set statements for Cross-Origin, Chrome gives a
// deprecation warning (April 2021) because shared memory requires
// cross origin isolation.  It is expected that Chrome 91 (May 2021)
// will refuse to create the shared array.

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

//-----------------------------------------------------------------------
// Testing the server
// URL path: hello.html
// URL path: world.html
//-----------------------------------------------------------------------

app.get ('/hello.html', (req,res) => {
    res.render ('hello')
})

app.get ('/world.html', (req,res) => {
    res.render ('world')
})

//-----------------------------------------------------------------------
// Launch the server
//-----------------------------------------------------------------------

// The build directory contains a set of subdirectories, one for each
// build that can be launched.  The subdirectories are named with the
// version number.  The server recognises two virtual versions:
// release and dev.  If either of these is launched, the server uses
// the specific version as defined in the environment variables
// S16_RELEASE_VERSION and S16_DEV_VERSION.  A comment at the
// beginning of this file gives the URLs needed to launch any version.

// if run env is Local, arg is the BUILD_DIR to use
export function StartServer (command) {
    console.log ('StartServer')
    let ok = true
    if (S16_RUN_ENV === 'Heroku') {
        console.log ('Running on Internet server')
        // Find the directory this program is running in and use that to
        // find the build directory
        S16_BUILD_DIR = path.join (S16_SERVER_DIR, '..', '..',
                                   'Sigma16', 'build')
    } else if (S16_RUN_ENV === 'Local') {
        console.log ('Running on local development machine')
        S16_LOCAL_BUILD_DIR = process.env.S16_LOCAL_BUILD_DIR
        S16_BUILD_DIR = command === "version"
            ? `${SIGSERVER_REPOSITORY}/Sigma16/build`
            : S16_LOCAL_BUILD_DIR
        console.log (`S16_LOCAL_BUILD_DIR = ${S16_LOCAL_BUILD_DIR}`)
        console.log (`S16_BUILD_DIR = ${S16_BUILD_DIR}`)
    } else {
        console.log (`Server error: cannot find build directory for `
                     + `${S16_RUN_ENV}`)
        ok = false
    }
    if (ok) {
        console.log ("Starting Sigma16 server")
        console.log (`command = ${command}`)
        console.log (`S16_RUN_ENV = ${S16_RUN_ENV}`)
        console.log (`S16_LATEST_RELEASE = ${S16_LATEST_RELEASE}`)
        console.log (`S16_RELEASE_VERSION = ${S16_RELEASE_VERSION}`)
        console.log (`S16_DEV_VERSION = ${S16_DEV_VERSION}`)
        console.log (`S16_SERVER_DIR = ${S16_SERVER_DIR}`)
        console.log (`S16_BUILD_DIR = ${S16_BUILD_DIR}`)
        app.listen(PORT, () => console.log
                   (`Server is listening on port ${PORT}`));
    }
}

// deprecated
//        S16_BUILD_DIR = S16_LOCAL_BUILD_DIR
//        S16_BUILD_DIR = path.join (process.env.SIGPART1,
//                                   process.env.SIGPART2,
//                                   process.env.SIGPART3,
//                                   'Sigma16', 'build')
//        console.log (`Local build directory = ${S16_LOCAL_BUILD_DIR}`)

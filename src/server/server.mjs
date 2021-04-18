// server.mjs
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

const LATEST_RELEASE = "3.2.2"
const PORT = process.env.PORT || 3000

// This program can run locally in a shell and launch the GUI in a
// browser.  It communicates with the browser using a port, which
// should be between 1024 and 49151.  The default is defined to be
// 3000; this can be changed to avoid clash with any other
// application.

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

// Find paths to components of the software, relative to this file

const cliDir = path.dirname (fileURLToPath (import.meta.url));
const homeDir = path.join (cliDir, "/../../")
const guiDir = path.join (homeDir, "/src/gui/")
const baseDir = path.join (homeDir, "/src/base/");
const compatDir = path.join (homeDir, "/src/compatibility/")
const datafilesDir = path.join (homeDir, "/src/datafiles/");
const docsDir = path.join (homeDir, "/docs/")
const examplesDir = path.join (homeDir, "/examples");

//----------------------------------------------------------------------------
// Server
//----------------------------------------------------------------------------

// Run express

const app = express();

// app.use (cors ())
app.set ('view engine', 'ejs')
app.use (express.static ('public'))



// Provide locations of static source files, docs, and examples
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
app.get('/', (req, res) => res.sendFile(guiDir + '/Sigma16.html'));

app.get('/gui/*', (req, res) => res.sendFile(guiDir));
app.get('/base/*', (req, res) => res.sendFile(baseDir));
app.get('/docs/*', (req, res) => res.sendFile(docsDir));
app.get('*', (req, res) => res.sendFile(guiDir));


//----------------------------------------------------------------------------
// Provide latest version on request
//----------------------------------------------------------------------------

// Launching Sigma16. When Sigma16 initializes, it makes an http fetch
// to /status/latest/i.j.k, where i.j.k identifies the running
// version.  The server logs the request and responds with a string
// giving the latest release.

// app.get ('/status/latest/*', cors (), (req,res) => {
app.get ('/status/latest/*', (req,res) => {
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
// Main program
//----------------------------------------------------------------------------

export function main () {
    console.log ('SigServer main')
    app.listen(PORT, () => console.log(`Listening on port ${PORT}`));
}

//----------------------------------------------------------------------------
// Deprecated
//----------------------------------------------------------------------------

/*

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


*/

// Sigma16.js
// Copyright (C) 2020 John T. O'Donnell
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
// Sigma16.mjs defines command line tools
// Usage: node Sigma16.mjs <command> <argument>
// Software requirements (install with npm): node.js, express
//-----------------------------------------------------------------------------

// Specify port to use for communicating with the browser
const port = 3000;  // this can be changed to avoid clash with any other app

// Summary of commands
//   - node Sigma16.mjs
//   - node Sigma16.mjs gui
//        Launch gui.  In browser visit http://localhost:3000/
//        Runs locally from downloaded files; it doesn't need Internet access.
//        Replace 3000 with the value of port defined above,
//   - node Sigma16.mjs assemble foo
//        Read foo.asm.txt and translate to machine language
//        Write foo.obj.txt, foo.lst.txt, foo.md.txt

//-----------------------------------------------------------------------------
// Initialization
//-----------------------------------------------------------------------------

// Load the required packages
import express from 'express';
import path from 'path';
import { fileURLToPath } from 'url';

// Obtain command line arguments
//    process.argv[0] = path to node.exe
//    process.argv[1] = path to this file

let command = process.argv[2]; // command to execute; if undefined use gui
let commandArg = process.argv[3]; // argument to the command, if any

// Find path to this module
const Sigma16directory = path.dirname(fileURLToPath(import.meta.url));

// For testing, print parameters
// console.log (`Sigma16: command=${command} arg=${commandArg}`);
// console.log (`Sigma16directory=${Sigma16directory}`);

//-----------------------------------------------------------------------------
// Initialize server for running gui
//-----------------------------------------------------------------------------

// Run express
const app = express();

// Provide locations of static source files, docs, and examples
app.use( '/', express.static(path.join(Sigma16directory + "/src/js")));
app.use( '/docs', express.static(path.join(Sigma16directory + "/docs")));
app.use( '/examples', express.static(path.join(Sigma16directory + "/examples")));

// Deliver proper mime types as required by CORS
express.static.mime.define({'application/javascript': ['js']});
express.static.mime.define({'text/css': ['css']});

// Respond to get requests by sending the requested files
app.get('/', (req, res) => res.sendFile(Sigma16directory + '/src/js/Sigma16.html'));
app.get('*', (req, res) => res.sendFile(Sigma16directory));

//-----------------------------------------------------------------------------
// Run gui in browser (visit http://localhost:<port>/)
//-----------------------------------------------------------------------------

// Start the server
function launchGUI () {
    app.listen(port, () => console.log(`Listening on port ${port}`));
    console.log (`Open http://localhost:${port}/ in your browser`);
}

//-----------------------------------------------------------------------------
// Main program
//-----------------------------------------------------------------------------

// for now, just gui. later, switch (command)
launchGUI ();

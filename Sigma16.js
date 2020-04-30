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
// Sigma16.js defines command line tools to run in a shell
// Usage: node Sigma16.js <cmd>
//   - node Sigma16.js gui    Launch gui, visit http://localhost:3000/
//-----------------------------------------------------------------------------

// Software requirements (install with npm): node.js, express

"use strict";

// Obtain command line arguments.  (process.argv[0] = path to
// node.exe, process.argv[1] = path to this file)
let command = process.argv[2]; // command to execute
let commandArg = process.argv[3]; // argument to the command, if any
// console.log (`Sigma16: command line args = ${process.argv}`);
console.log (`Sigma16: command=${command} arg=${commandArg}`);

// Specify port to use for communicating with the browser
const port = 3000;

// Load the required packages
const express = require('express');
const path = require("path");

// Run express
const app = express();

// Provide locations of static source files, docs, and examples
app.use( '/', express.static(path.join(__dirname + "/src/js")));
app.use( '/docs', express.static(path.join(__dirname + "/docs")));
app.use( '/examples', express.static(path.join(__dirname + "/examples")));

// Deliver proper mime types as required by CORS
express.static.mime.define({'application/javascript': ['js']});
express.static.mime.define({'text/css': ['css']});

// Respond to get requests by sending the requested files
app.get('/', (req, res) => res.sendFile(__dirname + '/src/js/Sigma16.html'));
app.get('*', (req, res) => res.sendFile(__dirname));

// Start the server
app.listen(port, () => console.log(`Listening on port ${port}!`));


/* from express hello world
app.get('/', (req, res) => res.send('Hello World!'))
app.use( '/src', express.static( __dirname + '/src' ));
app.get('*', (req, res) => {
  res.sendFile(path.join(__dirname + '/index.html'));
});
app.get('/', (req, res) => {
  res.sendFile(path.join(__dirname + "/Sigma16.html"));
});
app.listen(8080, () => console.log('Listening on port 8080!'));
*/


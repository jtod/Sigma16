// runserver.mjs
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

import * as serv from 'sigserver.mjs'

serv.StartServer ()

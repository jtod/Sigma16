// Sigma16: config.mjs
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

// Configuration: test browser compatibility

export function output (xs) {
    console.log (`config: output ${xs}`)
}

export function checkLocalStorageSupport () {
    return !!window.localStorage
}

export function checkBrowserWorkerSupport () {
    return !!window.Worker
}

export function checkSharedMemSupport () {
    return !!window.SharedArrayBuffer
}

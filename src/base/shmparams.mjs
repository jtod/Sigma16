// Sigma16: emulator.mjs
// Copyright (C) 2024 John T. O'Donnell.  License: GNU GPL Version 3
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

// Parameters for the shared memory

// The following sizes and offsets are measured in words

export const EmCtlSize        = 16
export const EmRegFileSize    = 16
export const EmSysRegSize     = 16
export const EmRegBlockSize   = EmRegFileSize + EmSysRegSize
export const EmMemSize        = 65536
export const EmStateSizeWord  = EmCtlSize + EmRegBlockSize + EmMemSize

export const EmRegBlockOffset = EmCtlSize
export const EmMemOffset      = EmRegBlockOffset + EmRegBlockSize

// The following sizes and offsets are measured in bytes

export const EmStateSizeByte  = 2 * EmStateSizeWord

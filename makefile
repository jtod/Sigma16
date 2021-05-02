# Sigma16: makefile
# Copyright (c) 2021 John T. O'Donnell, john.t.odonnell9@gmail.com
# License: GNU GPL Version 3 or later.  See README.org and LICENSE.txt

# This file is part of Sigma16.  Sigma16 is free software: you can
# redistribute it and/or modify it under the terms of the GNU General
# Public License as published by the Free Software Foundation, either
# version 3 of the License, or (at your option) any later version.
# Sigma16 is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.  You should have received
# a copy of the GNU General Public License along with Sigma16.  If
# not, see <https://www.gnu.org/licenses/>.

#-------------------------------------------------------------------------------
# Resources on the Internet
#-------------------------------------------------------------------------------

# You can run Sigma16 online, without downloading or installing
# anything: visit the Sigma16 home page and click the link:

#    https://jtod.github.io/home/Sigma16/

# The app won't run directly in the github source repository page:
# that will display the source code but won't render it.  Therefore,
# to run Sigma16 from the web, you need to use the homepage above.

# For additional tools, including the command line interface and
# circuit simulator, download from the source repositories and build
# the software.  See the Installation section in the User Guide

#    https://github.com/jtod/Sigma16
#    https://github.com/jtod/Hydra

#-------------------------------------------------------------------------------
# Usage
#-------------------------------------------------------------------------------

# Preparing the folder to run locally:
#    make set-version
#    After set-version, build html files from org using emacs export

# Additional preporation to run on Heroku:
#    make clear-build
#    make copy-build

# Copy files to Sigma16 homepage repository (on jtod.github.io)
#   make copybuild     copy the web release to the github homepage repository
#   make copyS16homepage

# Maintainance
#   make showconfig    print the configuration parameters
#   make clean         remove temporary files

#-------------------------------------------------------------------------------
# Configuration
#-------------------------------------------------------------------------------

# Most of the configuration paramaters are environment variables
# defined in .bashrc

# VERSION, the current version number, is extracted from the
# package.json file, on the line consisting of "version: : "1.2.3".
# VERSION is used for building the top level index and the user guide.

VERSION:=$(shell cat src/package.json | grep version | head -1 | awk -F= "{ print $2 }" | sed 's/[version:,\",]//g' | tr -d '[[:space:]]')

# Define the date in several formats for inclusion in the app and user guide

YEAR=$(shell date +"%Y")
MONTHYEAR=$(shell date +"%B %Y")
YEARMONTHDAY=$(shell date +"%F")
# MONTHYEARDAY=$(shell date +"%F")
# YEARMONTHDAY=$(shell date -I")

S16_CURRENT_BUILD_DIR="$(S16_LOCAL_BUILD_DIR)/$(S16_DEV_VERSION)/Sigma16"
# make showconfig - print out the configuration paramaters

.PHONY: showconfig
showconfig:
	@echo Environment variables
	@echo "  S16_LATEST_RELEASE = $(S16_LATEST_RELEASE)"
	@echo "  SIGMASYSTEM = $(SIGMASYSTEM)"
	@echo "  S16_LOCAL_BUILD_DIR = $(S16_LOCAL_BUILD_DIR)"
	@echo "  S16_DEV_VERSION = $(S16_DEV_VERSION)"
	@echo "  S16_DEV = $(S16_DEV)"
	@echo "  S16_CURRENT_BUILD_DIR = $(S16_CURRENT_BUILD_DIR)"
	@echo "  S16_LOCAL_PORT = $(S16_LOCAL_PORT)"
	@echo "  S16_RUN_ENV = $(S16_RUN_ENV)"
	@echo "  SIGSERVER = $(SIGSERVER)"
	@echo VERSION = $(VERSION)
	@echo MONTHYEAR = $(MONTHYEAR)
	@echo YEARMONTHDAY = $(YEARMONTHDAY)

#-------------------------------------------------------------------------------
# make set-version -- find version and define Version files
#-------------------------------------------------------------------------------

# make set-version --- The version number is defined in
# src/gui/package.json; this makefile finds the number there and
# defines a make variable $(VERSION).  This is used in several places,
# including writing a VERSION file in the top directory (used in the
# Welcome page and the User Guide) and src/gui/version.js (which makes
# the version number available to the JavaScript program).

VERSION.txt : src/package.json
	echo "Version $(VERSION), $(MONTHYEAR).\
	  Copyright (c) $(YEAR) John T. O'Donnell.  " \
	  > VERSION.txt

src/base/version.mjs : src/package.json
	echo "export const s16version = \"$(VERSION)\";" > src/base/version.mjs

#-------------------------------------------------------------------------------
# Build
#-------------------------------------------------------------------------------

# Remove all the files in build/(version). This is only necessary if a
# reorganization of the sources means that some redundant files would
# be left over; normally build-release will overwrite the files that
# have changed and leave the others unchanged.

.PHONY: clear-build
clear-build:
	/bin/rm -rf $(S16_CURRENT_BUILD_DIR)

# Copy the files needed to run the program from build/dev to
# build/(version). 

.PHONY: build
build:
	make VERSION.txt
	make src/base/version.mjs
	mkdir -p $(S16_CURRENT_BUILD_DIR)
	cp -u *.html $(S16_CURRENT_BUILD_DIR)
	cp -u *.txt  $(S16_CURRENT_BUILD_DIR)
	cp -ur docs $(S16_CURRENT_BUILD_DIR)
	cp -ur examples $(S16_CURRENT_BUILD_DIR)
	mkdir $(S16_CURRENT_BUILD_DIR)/src
	cp -ur src/gui $(S16_CURRENT_BUILD_DIR)/src
	cp -ur src/base $(S16_CURRENT_BUILD_DIR)/src

#-------------------------------------------------------------------------------
# Install server
#-------------------------------------------------------------------------------

# Copy server to Heroku source directory

.PHONY: install-server
install-server:
	cp -up src/server/sigserver.mjs .$(SIGSERVER)/src/server 

#-------------------------------------------------------------------------------
# Install home page
#-------------------------------------------------------------------------------

# Copy the home page index and style to the Sigma16 home page

.PHONY : install-home-page
install-home-page :
	mkdir -p $(S16HOMEPAGE)/admin
	cp -up protected/SIGSERVERURL.txt $(S16HOMEPAGE)/admin
	cp -up docs/S16homepage/index.html $(S16HOMEPAGE)
	cp -up docs/docstyle.css  $(S16HOMEPAGE)

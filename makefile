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

# make showconfig       display the configuration parameters
# make setVersion      update VERSION.txt and src/base/version.mjs
# emacs export org      build html files from org source after make setVersion
# make build            copy executable from dev source to build/i.j.k
# make installServer    copy server program to server repository
# make installBuild     copy dev build to server repository
# make installHomepage  copy homepage files to homepage repository

#-------------------------------------------------------------------------------
# Configuration
#-------------------------------------------------------------------------------

# Environment variables defined on Heroko
#  PAPERTRAIL_API_TOKEN

# Environment variables defined on both Heroko and local build machine:
#   S16_RUN_ENV           Heroku or Local
#   S16_LATEST_RELEASE    version number of latest official release
#   S16_RELEASE_VERSION   version number to use on request for 'release'

# Environment variables defined on local build machine:
#   S16_LOCAL_PORT      port for local server
#   SIGMASYSTEM         path to sources

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

# Define the directories for sources, builds, and repositories

S16_BUILDS_DIR=$(SIGMASYSTEM)/Sigma16-builds/build
S16_DEV_SRC_DIR=$(S16_BUILDS_DIR)/dev/Sigma16
S16_DEV_VERSION_DIR=$(S16_BUILDS_DIR)/$(VERSION)
S16_DEV_BUILD_DIR=$(S16_DEV_VERSION_DIR)/Sigma16
S16_HOMEPAGE_REPOSITORY=$(SIGMASYSTEM)/jtod.github.io/home/Sigma16
SIGSERVER_REPOSITORY=$(SIGMASYSTEM)/SigServer

ICLOUD=/cygdrive/C/Users/johnt/iCloudDrive

.PHONY: showconfig
showconfig:
	@echo "Environment variables"
	@echo "  S16_RUN_ENV = $(S16_RUN_ENV)"
	@echo "  S16_LATEST_RELEASE = $(S16_LATEST_RELEASE)"
	@echo "  S16_RELEASE_VERSION = $(S16_RELEASE_VERSION)"
	@echo "  S16_LOCAL_PORT = $(S16_LOCAL_PORT)"
	@echo "  SIGMASYSTEM = $(SIGMASYSTEM)"
	@echo "Calculated variables"
	@echo "  VERSION = $(VERSION)"
	@echo "  MONTHYEAR = $(MONTHYEAR)"
	@echo "  YEARMONTHDAY = $(YEARMONTHDAY)"
	@echo "  S16_BUILDS_DIR = $(S16_BUILDS_DIR)"
	@echo "  S16_DEV_SRC_DIR = $(S16_DEV_SRC_DIR)"
	@echo "  S16_DEV_VERSION_DIR = $(S16_DEV_VERSION_DIR)"
	@echo "  S16_DEV_BUILD_DIR = $(S16_DEV_BUILD_DIR)"
	@echo "  S16_HOMEPAGE_REPOSITORY = $(S16_HOMEPAGE_REPOSITORY)"
	@echo "  SIGSERVER_REPOSITORY = $(SIGSERVER_REPOSITORY)"

#-------------------------------------------------------------------------------
# make setVersion -- find version and define Version files
#-------------------------------------------------------------------------------

# make setVersion --- The version number is defined in
# src/gui/package.json; this makefile finds the number there and
# defines a make variable $(VERSION).  This is used in several places,
# including writing a VERSION file in the top directory (used in the
# Welcome page and the User Guide) and src/gui/version.js (which makes
# the version number available to the JavaScript program).  make
# setVersion should be invoked when the version or the Month/Year
# changes.

.PHONY: setVersion
setVersion:
	echo "export const s16version = \"$(VERSION)\";" > src/base/version.mjs
	echo "Version $(VERSION), $(MONTHYEAR)." > VERSION.txt

#	  Copyright (c) $(YEAR) John T. O'Donnell.  " \

#-------------------------------------------------------------------------------
# make assemble
#-------------------------------------------------------------------------------

.PHONY: assemble
assemble:
	make src/base/emcore.wasm

src/base/emcore.wasm: src/base/emcore.wat
	cd src/base; wat2wasm emcore.wat --enable-threads

#-------------------------------------------------------------------------------
# make build - Copy files needed to run program from build/dev to build/i.j.k
#-------------------------------------------------------------------------------

.PHONY: build
build:
	make VERSION.txt
	make src/base/version.mjs
	make src/base/emcore.wasm

	mkdir -p $(S16_DEV_BUILD_DIR)
	cp -u *.html $(S16_DEV_BUILD_DIR)
	cp -u *.txt  $(S16_DEV_BUILD_DIR)

	mkdir -p $(S16_DEV_BUILD_DIR)/src/gui
	cp -u src/gui/*.mjs $(S16_DEV_BUILD_DIR)/src/gui
	cp -u src/gui/*.css $(S16_DEV_BUILD_DIR)/src/gui

	mkdir -p $(S16_DEV_BUILD_DIR)/src/base
	cp -u src/base/*.mjs $(S16_DEV_BUILD_DIR)/src/base

	mkdir -p $(S16_DEV_BUILD_DIR)/docs
	cp -u docs/*.css $(S16_DEV_BUILD_DIR)/docs
	mkdir -p $(S16_DEV_BUILD_DIR)/docs/welcome
	cp -u docs/welcome/*.html $(S16_DEV_BUILD_DIR)/docs/welcome
	mkdir -p $(S16_DEV_BUILD_DIR)/docs/help
	cp -u docs/help/*.html $(S16_DEV_BUILD_DIR)/docs/help
	mkdir -p $(S16_DEV_BUILD_DIR)/docs/UserGuide
	cp -u docs/UserGuide/*.html $(S16_DEV_BUILD_DIR)/docs/UserGuide

	mkdir -p $(S16_DEV_BUILD_DIR)/examples
	cp -ur examples/* $(S16_DEV_BUILD_DIR)/examples

# If and when there are figure files, those will need to be copied too
#	mkdir -p $(S16_DEV_BUILD_DIR)/docs/UserGuide/png
#	cp -u docs/UserGuide/png/*.png $(S16_DEV_BUILD_DIR)/docs/UserGuide/png
#	mkdir -p $(S16_DEV_BUILD_DIR)/docs/UserGuide/svg
#	cp -u docs/UserGuide/svg/*.svg $(S16_DEV_BUILD_DIR)/docs/UserGuide/svg

#-------------------------------------------------------------------------------
# Install server
#-------------------------------------------------------------------------------

# Copy server to Heroku github source directory

.PHONY: installServer
installServer:
	cp -u src/server/sigserver.mjs $(SIGSERVER_REPOSITORY)/src/server 
	cp -u src/server/*.html $(SIGSERVER_REPOSITORY)/build
	cp -u docs/docstyle.css $(SIGSERVER_REPOSITORY)/build

#-------------------------------------------------------------------------------
# Install build
#-------------------------------------------------------------------------------

# Copy the dev build to the server repository

.PHONY: installBuild
installBuild:
	cp -upr $(S16_DEV_VERSION_DIR) $(SIGSERVER_REPOSITORY)/build

#-------------------------------------------------------------------------------
# Install home page
#-------------------------------------------------------------------------------

# Copy the home page index and style from dev source to the Sigma16
# home page repository.  From there it can be pushed to github.

.PHONY : installHomepage
installHomepage :
	mkdir -p $(S16_HOMEPAGE_REPOSITORY)/admin
	cp -u protected/SIGSERVERURL.txt $(S16_HOMEPAGE_REPOSITORY)/admin
	cp -u docs/S16homepage/index.html $(S16_HOMEPAGE_REPOSITORY)
	cp -u docs/docstyle.css  $(S16_HOMEPAGE_REPOSITORY)

.PHONY : icloud
icloud :
	mkdir -p $(ICLOUD)/writing-drafts/Sigma16
	cp docs/docstyle.css $(ICLOUD)/writing-drafts/Sigma16/
	cp -r docs/UserGuide $(ICLOUD)/writing-drafts/Sigma16/docs

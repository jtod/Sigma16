# Sigma16: makefile
# This file is part of Sigma16.  License: GNU GPL Version 3
# See Sigma16/README, LICENSE and https://jtod.github.io/home/Sigma16/
# Copyright (c) 2019-2022 John T. O'Donnell

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

#--------------------------------------------------------------------------
# Environment variables
#--------------------------------------------------------------------------

# S16_LOCAL_BUILD_DIR is the path to a directory that contains the
# source, which is xxx/build/dev/Sigma16.  The Sigma16 source is
# contained within xxx/build/dev so the server can assume that any
# version being executed, either locally or on the server, has a path
# of the form xxx/build/VERSIONNUMBER/Sigma16.  The source git
# directory is ${S16_LOCAL_BUILD_DIR}/dev/Sigma16, so it can be
# accessed using a "version" of "dev".

# S16_SERVER_SRC_BUILD_DIR is the target build directory where "make
# install-build" will put the files.  This is within the
# SigServer/Sigma16 directory, and will be uploaded to the server.  A
# particular version will have a path
# ${S16_SERVER_SRC_BUILD_DIR}/sigma16/build/VERSION/Sigma16

#--------------------------------------------------------------------------
# Resources on the Internet
#--------------------------------------------------------------------------

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

#--------------------------------------------------------------------------
# Usage
#--------------------------------------------------------------------------

# make setVersion    update VERSION.txt, COPYRIGHT.txt, src/base/version.mjs
# emacs org C-c C-E h h  build html from org source, 1st do make setVersion
# make showconfig        display the configuration parameters
# make assemble          assemble wat code
# make build             copy executable from dev source to build/i.j.k
# make installServer     copy server program to server repository
# make installBuild      copy dev build to server repository
# make installHomepage   copy homepage files to homepage repository

#--------------------------------------------------------------------------
# Configuration
#--------------------------------------------------------------------------

# Environment variables defined on Heroko
#  PAPERTRAIL_API_TOKEN

# Environment variables defined on both Heroko and local build machine:
#   S16_RUN_ENV           Heroku or Local
#   S16_LATEST_RELEASE    version number of latest official release
#   S16_RELEASE_VERSION   version number to use on request for 'release'
#   S16_DEV_VERSION       version number to use on request for 'dev'

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

# Source directory, contains development version
S16_DEV_SRC_DIR=$(S16_LOCAL_BUILD_DIR)/Sigma16

# Server repository, makefile copies a build to this location
SIGSERVER_REPOSITORY=$(SIGMASYSTEM)/server
S16_INSTALL_VERSION_DIR=$(SIGSERVER_REPOSITORY)/Sigma16/build/$(VERSION)
S16_INSTALL_DIR=$(S16_INSTALL_VERSION_DIR)/Sigma16

# Homepage repository
S16_HOMEPAGE_REPOSITORY=$(SIGMASYSTEM)/jtod.github.io/home/Sigma16

.PHONY: showconfig
showconfig:
	@echo "Environment variables"
	@echo "  S16_RUN_ENV = $(S16_RUN_ENV)"
	@echo "  S16_LATEST_RELEASE = $(S16_LATEST_RELEASE)"
	@echo "  S16_RELEASE_VERSION = $(S16_RELEASE_VERSION)"
	@echo "  S16_DEV_VERSION = $(S16_DEV_VERSION)"
	@echo "  S16_LOCAL_PORT = $(S16_LOCAL_PORT)"
	@echo "  SIGMASYSTEM = $(SIGMASYSTEM)"
	@echo "  S16_LOCAL_BUILD_DIR = $(S16_LOCAL_BUILD_DIR)"
	@echo "  S16_SERVER_SRC_BUILD_DIR = $(S16_SERVER_SRC_BUILD_DIR)"
	@echo "Calculated variables"
	@echo "  VERSION = $(VERSION)"
	@echo "  MONTHYEAR = $(MONTHYEAR)"
	@echo "  YEARMONTHDAY = $(YEARMONTHDAY)"
	@echo "  S16_DEV_SRC_DIR = $(S16_DEV_SRC_DIR)"
	@echo "  S16_INSTALL_DIR = $(S16_INSTALL_DIR)"
	@echo "  S16_HOMEPAGE_REPOSITORY = $(S16_HOMEPAGE_REPOSITORY)"
	@echo "  SIGSERVER_REPOSITORY = $(SIGSERVER_REPOSITORY)"

#--------------------------------------------------------------------------
# Check documents
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
# Preparing a release
#--------------------------------------------------------------------------

# make setVersion

# emacs: update html for README.org, docs/welcome/welcome.org,
# docs/UserGUide/Sigma16UserGuide.org

# emacs: update html for docs/help/*.org

# HomePage doesn't need to be updated for a release; only when the
# homepage text is changed. emacs: update html for
# docs/S16Homepage/index.org

# $ git tag -a v3.4.3 -m "version 3.4.3"
# $ git push --tags

#--------------------------------------------------------------------------
# make setVersion -- find version and define Version files
#--------------------------------------------------------------------------

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
	echo "Copyright (c) $(YEAR) John T. O'Donnell" > COPYRIGHT.txt

#--------------------------------------------------------------------------
# make assemble
#--------------------------------------------------------------------------

.PHONY: assemble
assemble:
	make src/base/emcore.wasm

src/base/emcore.wasm: src/base/emcore.wat
	echo Skipping wat2wasm
#	cd src/base; wat2wasm emcore.wat --enable-threads

#--------------------------------------------------------------------------
# make compile -- compile in src directory
#--------------------------------------------------------------------------

.PHONY: compile
compile:
	@echo Compiling
	make setVersion
	make assemble

#--------------------------------------------------------------------------
# make build -- compile and install into local server repository
#--------------------------------------------------------------------------

checkInstallDir:
	@echo S16_INSTALL_VERSION_DIR = $(S16_INSTALL_VERSION_DIR)
	@echo S16_INSTALL_DIR = $(S16_INSTALL_DIR)
	cd $(S16_INSTALL_DIR); pwd; ls


.PHONY: build
build:
	@echo Installing in $(S16_INSTALL_DIR)

	@echo backup $(S16_INSTALL_VERSION_DIR)
	touch $(S16_INSTALL_VERSION_DIR)
	mv -i $(S16_INSTALL_VERSION_DIR) $(S16_INSTALL_VERSION_DIR)-bak

	make setVersion
#	make assemble
	mkdir -p -m700 $(S16_INSTALL_DIR)

	cp -u *.html $(S16_INSTALL_DIR)
	cp -u *.txt  $(S16_INSTALL_DIR)

	mkdir -p -m700 $(S16_INSTALL_DIR)/src/gui
	cp -u src/gui/*.mjs $(S16_INSTALL_DIR)/src/gui
	cp -u src/gui/*.css $(S16_INSTALL_DIR)/src/gui

	mkdir -p -m700 $(S16_INSTALL_DIR)/src/base
	cp -u src/base/*.mjs $(S16_INSTALL_DIR)/src/base

	mkdir -p -m700 $(S16_INSTALL_DIR)/docs
	chmod u+rwx $(S16_INSTALL_DIR)/docs
	cp -u docs/*.css $(S16_INSTALL_DIR)/docs
	mkdir -p -m700 $(S16_INSTALL_DIR)/docs/welcome
	cp -u docs/welcome/*.html $(S16_INSTALL_DIR)/docs/welcome
	mkdir -p -m700 $(S16_INSTALL_DIR)/docs/help
	cp -u docs/help/*.html $(S16_INSTALL_DIR)/docs/help
	mkdir -p -m700 $(S16_INSTALL_DIR)/docs/UserGuide
	cp -u docs/UserGuide/*.html $(S16_INSTALL_DIR)/docs/UserGuide

	mkdir -p -m700 $(S16_INSTALL_DIR)/examples
	cp -ur examples/* $(S16_INSTALL_DIR)/examples

#--------------------------------------------------------------------------
# Install server
#--------------------------------------------------------------------------

# Copy server to Heroku github source directory.  Using quotes around
# $(SIGSERVER_REPOSITORY) in case it contains spaces

.PHONY: installServer
installServer:
	cp -u src/server/runserver.mjs $(SIGSERVER_REPOSITORY)/src/server
	cp -u src/server/sigserver.mjs $(SIGSERVER_REPOSITORY)/src/server

#--------------------------------------------------------------------------
# Install home page
#--------------------------------------------------------------------------

# Copy the home page index and style from dev source to the Sigma16
# home page repository.  From there it can be pushed to github.

.PHONY : installHomepage
installHomepage :
	mkdir -p $(S16_HOMEPAGE_REPOSITORY)/admin
	cp -u protected/SIGSERVERURL.txt $(S16_HOMEPAGE_REPOSITORY)/admin
	cp -u docs/S16homepage/index.html $(S16_HOMEPAGE_REPOSITORY)
	cp -u docs/docstyle.css  $(S16_HOMEPAGE_REPOSITORY)

#--------------------------------------------------------------------------
# M1
#--------------------------------------------------------------------------

M1doc :
	cd src/circuits; haddock --html --hyperlinked-source --odir=doc HDL/Hydra/Circuits/* HDL/Hydra/Core/* M1/Circuit/*.hs M1/Tools/*.hs


# deprecated...

#--------------------------------------------------------------------------
# Install build: copy the dev build to the local server repository
#--------------------------------------------------------------------------
# .PHONY: installBuild
# installBuild:
# 	cp -upr $(S16_DEV_SRC_DIR) $(SIGSERVER_REPOSITORY)/build/$(VERSION)

# If and when there are figure files, those will need to be copied too
#	mkdir -p $(S16_INSTALL_DIR)/docs/UserGuide/png
#	cp -u docs/UserGuide/png/*.png $(S16_INSTALL_DIR)/docs/UserGuide/png
#	mkdir -p $(S16_INSTALL_DIR)/docs/UserGuide/svg
#	cp -u docs/UserGuide/svg/*.svg $(S16_INSTALL_DIR)/docs/UserGuide/svg

# S16_BUILDS_DIR=$(SIGMASYSTEM)/Sigma16-builds/build
# S16_DEV_BUILD_DIR=$(S16_DEV_VERSION_DIR)/Sigma16
#	@echo "  S16_BUILDS_DIR = $(S16_BUILDS_DIR)"
#	@echo "  S16_DEV_BUILD_DIR = $(S16_DEV_BUILD_DIR)"
#	@echo "  S16_DEV_VERSION_DIR = $(S16_DEV_VERSION_DIR)"
# : the directory containing this makefile
# Copy build to
# deprecated...
#	make VERSION.txt
#	make src/base/version.mjs

#	cp -u src/server/*.html $(SIGSERVER_REPOSITORY)/build
#	cp -u docs/docstyle.css $(SIGSERVER_REPOSITORY)/build

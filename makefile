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

# make showconfig - print out the configuration paramaters

.PHONY: showconfig
showconfig:
	@echo Environment variables
	@echo "  S16_LATEST_RELEASE = $(S16_LATEST_RELEASE)"
	@echo "  S16_TEST_VERSION = $(S16_TEST_VERSION)"
	@echo "  S16_DEV_VERSION = $(S16_DEV_VERSION)"
	@echo "  SIGMASYSTEM = $(SIGMASYSTEM)"
	@echo "  S16_LOCAL_BUILD_DIR = $(S16_LOCAL_BUILD_DIR)"
	@echo "  S16_DEV = $(S16_DEV)"
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

.PHONY : set-version
set-version :
	echo "Version $(VERSION), $(MONTHYEAR).\
	  Copyright (c) $(YEAR) John T. O'Donnell.  " \
	  > VERSION.txt
	echo "export const s16version = \"$(VERSION)\";" > src/base/version.mjs

#-------------------------------------------------------------------------------
# Build dev version on local machine
#-------------------------------------------------------------------------------

# Use 'make set-version' to ensure the version files are consistent
# with src/package.json

# Remove all the files in build/(version). This is only necessary if a
# reorganization of the sources means that some redundant files would
# be left over; normally build-release will overwrite the files that
# have changed and leave the others unchanged.

.PHONY: clear-dev
clear-build:
	echo /bin/rm -rf $(S16_DEV)/*

# Copy the files needed to run the program from build/dev to
# build/(version). 

.PHONY: copy-dev
copy-build:
	mkdir -p $(S16_DEV)/src/gui
	mkdir -p $(S16_DEV)/src/base
	cp -up *.html $(S16_DEV)
	cp -up *.txt  $(S16_DEV)
	cp -upr src/gui/*.mjs  ../SigServer/build/Sigma16/src/gui
	cp -upr src/base/*.mjs ../SigServer/build/Sigma16/src/base
	mkdir $(S16_DEV)/docs/welcome
	mkdir $(S16_DEV)/docs/help
	mkdir $(S16_DEV)/docs/UserGuide
	cp -upr docs ../SigServer/build/Sigma16/
	cp -upr examples ../SigServer/build/Sigma16/

#-------------------------------------------------------------------------------
# Copy server
#-------------------------------------------------------------------------------

# Copy server to Heroku source directory

.PHONY: copy-server
copy-server:
	cp -up src/server/sigserver.mjs .$(SIGSERVER)/src/server 


# Don't need this
.PHONY: build-release-DEPRECATED
build-release-DEPRECATED:
	echo VERSIONPATH=$(VERSIONPATH)
	mkdir -p $(VERSIONPATH)
	cp -up README.org $(VERSIONPATH)
	cp -up LICENSE.txt $(VERSIONPATH)
	cp -up VERSION.txt $(VERSIONPATH)
	cp -up LATESTVERSION.txt $(VERSIONPATH)
	cp -up Sigma16.html $(VERSIONPATH)
	mkdir -p $(VERSIONPATH)/src/gui
	mkdir -p $(VERSIONPATH)/src/base
	cp -upr src/gui/*.mjs $(VERSIONPATH)/src/gui
	cp -upr src/gui/*.css $(VERSIONPATH)/src/gui
	cp -upr src/base/*.mjs $(VERSIONPATH)/src/base
	mkdir -p $(VERSIONPATH)/docs/Welcome
	mkdir -p $(VERSIONPATH)/docs/help
	mkdir -p $(VERSIONPATH)/docs/UserGuide
	cp -up docs/docstyle.css $(VERSIONPATH)/docs
	cp -up docs/Welcome/*.html $(VERSIONPATH)/docs/Welcome
	cp -up docs/help/*.html $(VERSIONPATH)/docs/help
	cp -upr docs/UserGuide/*.html $(VERSIONPATH)/docs/UserGuide
	cp -upr examples $(VERSIONPATH)
	echo build-release finished

.PHONY: webbuild
webbuild:
	make set-version
	mkdir -p $(WEBBUILD)/$(VERSION)
	/bin/rm -rf $(WEBBUILD)/$(VERSION)/*
	cp -up README.org $(WEBBUILD)/$(VERSION)
	cp -up LICENSE.txt $(WEBBUILD)/$(VERSION)
	cp -up VERSION.txt $(WEBBUILD)/$(VERSION)
	cp -up LATESTVERSION.txt $(WEBBUILD)/$(VERSION)

	cp -upr examples $(WEBBUILD)/$(VERSION)

	mkdir -p $(WEBBUILD)/$(VERSION)/docs
	cp -up docs/*.css $(WEBBUILD)/$(VERSION)/docs

	mkdir -p $(WEBBUILD)/$(VERSION)/docs/help
	cp -up docs/help/*.html $(WEBBUILD)/$(VERSION)/docs/help

	mkdir -p $(WEBBUILD)/$(VERSION)/docs/Welcome
	cp -up docs/Welcome/*.html $(WEBBUILD)/$(VERSION)/docs/Welcome

	mkdir -p $(WEBBUILD)/$(VERSION)/docs/UserGuide
	cp -up docs/UserGuide/*.html $(WEBBUILD)/$(VERSION)/docs/UserGuide
	cp -upr docs/UserGuide/png $(WEBBUILD)/$(VERSION)/docs/UserGuide
	cp -upr docs/UserGuide/svg $(WEBBUILD)/$(VERSION)/docs/UserGuide

	mkdir -p $(WEBBUILD)/$(VERSION)/src/base
	mkdir -p $(WEBBUILD)/$(VERSION)/src/gui
	cp -upr src/gui/*.mjs $(WEBBUILD)/$(VERSION)/src/gui
	cp -upr src/gui/*.html $(WEBBUILD)/$(VERSION)/src/gui
	cp -upr src/gui/*.css $(WEBBUILD)/$(VERSION)/src/gui
	cp -upr src/base/*.mjs $(WEBBUILD)/$(VERSION)/src/base

# cp -upr src/datafiles/*.html $(WEBBUILD)/$(VERSION)/src/datafiles
#	cp -up docs/docstyle.css $(WEBBUILD)/$(VERSION)/docs
#	cp docs/UserGuide/*.css $(WEBBUILD)/$(VERSION)/docs/UserGuide
#	cp -upr src/datafiles/*.css $(WEBBUILD)/$(VERSION)/src/datafiles

#-------------------------------------------------------------------------------
# Copy the build into S16 home repository
#-------------------------------------------------------------------------------

# Build the system and copy files to git repository for Sigma16 home page

all :
	make build
	make homepage

# Build the development version and copy to homepage repository
.PHONY : build
build :
	make build-release

#	make copybuild
#	make webbuild

# Build the homepage and copy to homepage repository
homepage :
	make copyS16homepage
	make copytesting

# Copy the current release buffer into the local repository for the
# github home page

.PHONY : copybuild
copybuild :
	echo $(S16HOMEPAGE)/releases
	ls $(S16HOMEPAGE)/releases
	/bin/rm -rf $(S16HOMEPAGE)/releases/${VERSION}
	cp -r $(WEBBUILD)/$(VERSION) $(S16HOMEPAGE)/releases

# Copy the home page index and style to the Sigma16 home page

.PHONY : copyS16homepage
copyS16homepage :
	mkdir -p $(S16HOMEPAGE)/admin
	cp -up protected/SIGSERVERURL.txt $(S16HOMEPAGE)/admin
	cp -up docs/S16homepage/index.html $(S16HOMEPAGE)
	cp -up docs/docstyle.css  $(S16HOMEPAGE)
#	cp -up docs/S16homepage/index.css  $(S16HOMEPAGE)

# Copy the testing files to the Sigma16 home page

.PHONY : copytesting
copytesting :
	mkdir -p $(S16HOMEPAGE)/testing/compatibility
	/bin/rm -rf $(S16HOMEPAGE)/testing/compatibility/*
	cp -r src/compatibility/*.html $(S16HOMEPAGE)/testing/compatibility
	cp -r src/compatibility/*.mjs $(S16HOMEPAGE)/testing/compatibility

#-------------------------------------------------------------------------------
# make clean -- remove backup and temp files
#-------------------------------------------------------------------------------

.PHONY : clean
clean :
	find . \( -name '*~' -o -name '*.bak' -o \
	          -name '*.lst.txt' -o -name '*.obj.txt' -o \
	          -name '*.omd.txt' -o -name '*.exe.txt' -o \
	          -name '*.xmd.txt' -o -name '*.xlt.txt' -o \
	          -name '*.o' -o -name '*.hi' \) \
	       -delete

.PHONY : log
log :
	papertrail status/latest > protected/logs/$(YEARMONTHDAY).txt

#-------------------------------------------------------------------------------
# Build standalone program on local machine using electron
#-------------------------------------------------------------------------------

# Use electron-builder to generate a native executable for the current
# platform.  This allows the program to be launched by clicking the
# executable, and it isn't necessary to have internet access, or to
# have npm or the other software tools installed.

# Install npm and dependencies
#   https://electronjs.org/docss/tutorial/installation
#   install npm from the electronjs.org web page
#   npm install                       -- install dependencies
#   npm install electron --save-dev   -- install electron
#   npm start                         -- run on local machine
#   npm run mkdist                    -- build standalone executable
# Install dependencies: cd app; npm install
# Build executable: cd app; npm run mkdist
# To run: cd app; npm start
# Move executable: mv src/gui/dist/*.exe Sigma16-$(VERSION)-win.exe

# There is a bug in Electron-builder: it gives a bad name to the exe
# file; for example it produces 'sigma16 3.0.1-7.2.exe' including the
# quote characters, and if a better name is specified using
# artifactName it fails to expand the variables.  So in building the
# release, the executable files should be renamed as they are moved.

#-------------------------------------------------------------------------------
# Deprecated
#-------------------------------------------------------------------------------

# Build and copy development version
#  make all
# Build system in ${SIGMASYSTEM}/Sigma16
#   make webbuild      build a web release in the ./build directory

# SIGMASYSTEM is the path to parent of the directory containing this makefile
# SIGMASYSTEM:=./..
#	ls $(SIGMASYSTEM)
#	ls $(SIGSERVER)

# The executable web version is created in WEBBUILD and later uploaded to
# github in the releases directory
# WEBBUILD=./build
#	echo WEBBUILD = $(WEBBUILD)
#	echo VERSION = $(VERSION)

# S16HOMEPAGE is the local source repository for the Sigma16 Home
# Page.  This can be pushed to github.
# S16HOMEPAGE:=$(SIGMASYSTEM)/jtod.github.io/home/Sigma16
#	@echo S16HOMEPAGE = $(S16HOMEPAGE)

# SIGSERVER=SIGMASYSTEM/SigServer
#	cp -up src/server/sigserver.mjs ../SigServer/src/server 
# Don't need this...
# VERSIONPATH=build/Sigma16/release/$(VERSION)/Sigma16

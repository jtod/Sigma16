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

# make webbuild      build a web release in the ./build directory
# make copybuild     copy the web release to the github homepage repository
# make set-version   find version from package.json and write VERSION.txt
# make showparams    print the version, file locations, date, etc
# make clean         remove temporary files

#-------------------------------------------------------------------------------
# Define parameters
#-------------------------------------------------------------------------------

# SIGMASYSTEM is the path to parent of the directory containing this makefile

SIGMASYSTEM:=./..

# S16HOMEPAGE is the local source repository for the Sigma16 Home
# Page.  This can be pushed to github.

S16HOMEPAGE:=$(SIGMASYSTEM)/homepage/jtod.github.io/home/Sigma16

# The executable web version is created in WEBBUILD and later uploaded to
# github in the releases directory

WEBBUILD=./build

# VERSION is the current version number.  It's extracted from the
# package.json file, on the line consisting of "version: : "1.2.3".
# VERSION is used for building the top level index and the user guide.

VERSION:=$(shell cat src/package.json | grep version | head -1 | awk -F= "{ print $2 }" | sed 's/[version:,\",]//g' | tr -d '[[:space:]]')

# Define the date in several formats for inclusion in the app and user guide

YEAR=$(shell date +"%Y")
MONTHYEAR=$(shell date +"%B %Y")
MONTHYEARDAY=$(shell date +"%F")

# make showparams - print out the defined values

.PHONY: showparams
showparams:
	echo SIGMASYSTEM = $(SIGMASYSTEM)
	echo S16HOMEPAGE = $(S16HOMEPAGE)
	echo WEBBUILD = $(WEBBUILD)
	echo VERSION = $(VERSION)
	echo MONTHYEAR = $(MONTHYEAR)
	echo MONTHYEARDAY = $(MONTHYEARDAY)
	echo YEAR = $(YEAR)

#-------------------------------------------------------------------------------
# For users: build the command line tools
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# For developer: build a web release for the Sigma16 Home Page
#-------------------------------------------------------------------------------

# Although JavaScript can be executed directly in a browser, there are
# some files that need to be built before uploading to the Sigma16
# home page.  These include parsing the current version number (from
# package.json) and generating the user html pages from org source.

# Once the files are built, the system can be given a release number
# (as a tag) and uploaded to the Sigma16 project page for download.
# The release itself is then copied to the
# jtod.github.io/Sigma16/releases page for running the app directly
# from the web, without downloading anything.

# Update "version" in src/package.json
# Build html files from org source using emacs
#    src/datafiles/welcome.org
#     docs/S16homepage/index.html
#     example-indices
#     docs/src/S16homepage/index.org
# make webbuild -- prepare the release in build/
# commit and push
# git tag -a v3.0.26 -m 'version v3.0.26'

# Prepare the SigmaSystem homepage in local directory
#     make copybuild -- copy the release from build to the homepage repository
#     Edit ... /jtod.github.io/home/Sigma16/README to point to the new release
#     git push

#-------------------------------------------------------------------------------
# make webbuild -- prepare the release in build directory
#-------------------------------------------------------------------------------

#     Build html files from org source using emacs
#     make set-version
#     make release
#     git commit -m "S16 <release number>"
#     git tag -a v3.0.26 -m 'move to version v3.0.26'

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

	mkdir -p $(WEBBUILD)/$(VERSION)/docs/UserGuide
	cp docs/UserGuide/*.css $(WEBBUILD)/$(VERSION)/docs/UserGuide
	cp docs/UserGuide/*.html $(WEBBUILD)/$(VERSION)/docs/UserGuide
	cp -pr docs/UserGuide/png $(WEBBUILD)/$(VERSION)/docs/UserGuide
	cp -pr docs/UserGuide/svg $(WEBBUILD)/$(VERSION)/docs/UserGuide

	mkdir -p $(WEBBUILD)/$(VERSION)/src/base
	mkdir -p $(WEBBUILD)/$(VERSION)/src/gui
	mkdir -p $(WEBBUILD)/$(VERSION)/src/datafiles
	cp -upr src/gui/*.mjs $(WEBBUILD)/$(VERSION)/src/gui
	cp -upr src/gui/*.html $(WEBBUILD)/$(VERSION)/src/gui
	cp -upr src/gui/*.css $(WEBBUILD)/$(VERSION)/src/gui
	cp -upr src/base/*.mjs $(WEBBUILD)/$(VERSION)/src/base
	cp -upr src/datafiles/*.css $(WEBBUILD)/$(VERSION)/src/datafiles
	cp -upr src/datafiles/*.html $(WEBBUILD)/$(VERSION)/src/datafiles

#-------------------------------------------------------------------------------
# make copybuild - copy the build into S16 home repository
#-------------------------------------------------------------------------------

# Copy the current release buffer into the local repository for the
# github home page

.PHONY : copybuild
copybuild :
	echo $(S16HOMEPAGE)/releases
	ls $(S16HOMEPAGE)/releases
	/bin/rm -rf $(S16HOMEPAGE)/releases/${VERSION}
	cp -r $(WEBBUILD)/$(VERSION) $(S16HOMEPAGE)/releases

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

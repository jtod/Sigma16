# Sigma16: makefile
# Copyright (c) 2020 John T. O'Donnell  john.t.odonnell9@gmail.com
# License: GNU GPL Version 3 or later.  Sigma16/LICENSE.txt,NOTICE.txt

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
# Resources
#-------------------------------------------------------------------------------

# You can run Sigma16 online, without downloading or installing
# anything, go to Sigma16 home page and click the link.  For
# additional tools, including command line interface and circuit
# simulator, download from the source repositories.  See the
# Installation section in the User Guide

# Web links
#   Sigma16 home page:          https://jtod.github.io/home/Sigma16/
#   Sigma16 source repository:  https://github.com/jtod/Sigma16
#   Hydra source repository:    https://github.com/jtod/Hydra

# Source file directories
#   SigmaSystem/Sigma16
#   SigmaSystem/homepage/jtod.github.io/home/Sigma16

#-------------------------------------------------------------------------------
# Usage
#-------------------------------------------------------------------------------

# Manual steps to perform in emacs
#  index.html - export README.md

# make tools for building
#     make set-version   read version from package.json, write version files

#     make devversion                               prepare home/Sigma16/dev

# git and github
#     git status, git add, git commit, git push

# Make release                              prepare home/Sigma16/releases/...
#     Update version in Sigma16/package.json
#     make devversion                               prepare home/Sigma16/dev
#     git status, git add, git commit, git push
#     cd SigmaSystem/homepage/jtod.github.io/home/Sigma16
#     edit ...
#     make docs/src/S16homepage/index.html       index for Sigma16 home page
#     git status, git add, git commit, git push

# Update home page on github


#-------------------------------------------------------------------------------
# Usage
#-------------------------------------------------------------------------------

# Build the IDE app
#   make release            build web page for posting on Internet
#   make compile            build executable using npm to compile from source

# Needed to build both web and compiled version
#   make set-version        get version number from package.json
#   make source-dir-index          generate html from markdown source
#   make example-indices    index for each directory in programs and examples

# Needed for compilation by npm
#   make dependencies       use npm to download Javascript dependencies
#   make run                run locally
#   make release            make a directory for publication
#   make executable         package up the code into a native executable
#   make move-exe           move the executable into release

#-------------------------------------------------------------------------------
# Files
#-------------------------------------------------------------------------------

# The following files are written by various compilation tools; they
# are not source and shouldn't be edited.  The files marked optional
# are produced when a standalone version is generated, but are not
# necessaryand can be deleted.

#  dist                directory produced by npm (optional)
#  node_modules        directory of packages downloaed by npm (optional)
#  package-lock.json   records package versions; produced by npm (optional)
#  version.js          written by make set-version
#  docs/html            written by make docs

#-------------------------------------------------------------------------------
# Make a release for posting on the web
#-------------------------------------------------------------------------------

# Although JavaScript can be executed directly in a browser, there are
# some files that need to be built.  These include parsing the current
# version number (from package.json), generating the user guide and
# other html pages from markdown source.

# Once the files are all built, the system can be given a release
# number (as a tag) and uploaded to the Sigma16 project page for
# download.  The release itself is then copied to the
# jtod.github.io/Sigma16/releases page for running the app directly
# from the web, without downloading anything.

# (1) Local editing in Sigma/current/Sigma16
#    a. Edit version number in package.json
#    b. make release
#    c. git status, git add...
#    d. git commit -m "S16 <release number>"

# (2) Upload to project page for users to download
#    a. Upload to project page: git push origin master
#    b. Visit https://github.com/jtod/Sigma16
#    b. Click New pull request
#    c. Merge the commit and confirm

# (2) Upload executable version In Sigma/current/homepage/jtod.github.io
#    a. Edit S16/index.md to point to the new release
#    b. git status, git add...
#    d. git push origin releases

# (3) On jtod.github.io
#    a. pull: compare master with releases (select releases from dropdown)
#    b. pull request
#    c. merge and confirm

#-------------------------------------------------------------------------------
# Define parameters
#-------------------------------------------------------------------------------

# Run the makefile from SigmaSystem/Sigma16

# Path to parent of the directory containing this makefile
SIGMAPROJECT:=./..

# Source for the Sigma16 Home Page
S16HOMEPAGESOURCE:=$(SIGMAPROJECT)/homepage/jtod.github.io/home/Sigma16
RELEASEDEVELOPMENT:=$(S16HOMEPAGESOURCE)/dev

# S16HOMEPAGE is a directory in my homepage on github; this is where
# the web release is placed, since users can run the app by clicking a
# link pointing into this area

S16HOMEPAGE:=$(SIGMAPROJECT)/homepage/jtod.github.io/home/Sigma16

# Extract the version from the package.json file; it's on the line
# consisting of "version: : "1.2.3".  This defines VERSION, which is
# used for building the top level index and the user guide.

VERSION:=$(shell cat src/package.json | grep version | head -1 | awk -F= "{ print $2 }" | sed 's/[version:,\",]//g' | tr -d '[[:space:]]')

# Define dates in several formats, for inclusion in the app and user guide
YEAR=$(shell date +"%Y")
MONTHYEAR=$(shell date +"%B %Y")
MONTHYEARDAY=$(shell date +"%F")

MDVERSION="Version $(VERSION), $(MONTHYEAR)"
MDCOPYRIGHT="Copyright $(YEAR) John T. O&apos;Donnell"
MDLATEST="For latest version, see <a href='https://jtod.github.io/home/Sigma16/' target='_blank'>https://jtod.github.io/home/Sigma16/</a>"

MDHEADER=$(MDVERSION).\ $(MDCOPYRIGHT).\ $(MDLATEST).

# SIGMAPROJECT         project directory; parent of Sigma16 directory
# S16HOMEPAGE          location of source for the Sigma16 Home Page
# VERSION              version number, found by looking at Sigma16/package.json
# MONTHYEAR            date for display in the gui and in the User Guide
# RELEASEDEVELOPMENT   directory for the release in development

# make showparams - print out the defined values
.PHONY: showparams
showparams:
	echo $(MDVERSION)
	echo ${MDCOPYRIGHT}
	echo $(MDLATEST)
	echo MDHEADER = ${MDHEADER}
	echo SIGMAPROJECT = $(SIGMAPROJECT)
	echo S16HOMEPAGE = $(S16HOMEPAGE)
	echo VERSION = $(VERSION).txt
	echo MONTHYEAR = $(MONTHYEAR)
	echo MONTHYEARDAY = $(MONTHYEARDAY)
	echo YEAR = $(YEAR)
	echo RELEASEDEVELOPMENT = $(RELEASEDEVELOPMENT)


#-------------------------------------------------------------------------------
# Development version
#-------------------------------------------------------------------------------

# make devversion -- build the documentation files, and copy the files
# to the web page current development directory.  After doing this,
# git status, git add, git commit, git push

# Build files that require preprocessing or compilation from source

.PHONY: build
build:
	make set-version
	make source-dir-index
	make src/datafiles/welcome.html
	make example-indices
	make docs/html/S16homepage/index.html

# Copy the dev version to Sigma16 homepage.  Make directory containing
# files for current development version, for uploading to the Sigma16
# home page.

.PHONY: release-development
release-development:
	mkdir -p $(RELEASEDEVELOPMENT)
	/bin/rm -rf $(RELEASEDEVELOPMENT)/*
	mkdir -p $(RELEASEDEVELOPMENT)/src
	mkdir -p $(RELEASEDEVELOPMENT)/src/Sigma16
	mkdir -p $(RELEASEDEVELOPMENT)/docs
	mkdir -p $(RELEASEDEVELOPMENT)/docs/html
	cp -up VERSION.txt $(RELEASEDEVELOPMENT)
	cp -up LICENSE.txt $(RELEASEDEVELOPMENT)
	cp -upr docs/ $(RELEASEDEVELOPMENT)
	cp -upr examples $(RELEASEDEVELOPMENT)
	cp -upr src/gui $(RELEASEDEVELOPMENT)/src
	cp -upr src/datafiles $(RELEASEDEVELOPMENT)/src
	cp -upr src/Sigma16/base $(RELEASEDEVELOPMENT)/src/Sigma16
	cp -up docs/html/S16homepage/index.html $(S16HOMEPAGE)
	cp -up docs/src/S16homepage/homepage.css $(S16HOMEPAGE)

# make release -- create a directory containing the source release of
# the current version.  The app can be launched by clicking a link,
# without needing to download anything.  This simply retains the dev
# version in the S16 Home Page, and copies it into releases using the
# VERSION as the folder name.

# When getting ready to make release, before doing the make
# devversion, be sure to check docs/src/S16homepage/index.md.  This
# contains two links to the latest release, one in the "Click to run"
# link and the other in the User Guide link.  Both of these need to be
# updated.

.PHONY : release
release :
	cp -r $(S16HOMEPAGESOURCE)/dev $(S16HOMEPAGESOURCE)/releases/$(VERSION)

#-------------------------------------------------------------------------------
# Running Sigma16
#-------------------------------------------------------------------------------

# You can run Sigma16 in several ways:

#  (1) Visit the web page jtod.github.io/S16 and click on the link to
#      the latest version.  You need to be connected to the Internet.
#      (If you want the source code, visit jtod.github.io/Sigma16
#      where you can read or download the source, but the app will not
#      run from that location).

#  (2) Download the source files to your local machine and visit
#      index.html in a browser.  You don't need to be connected to the
#      Internet, but a few features won't work: When you open one of
#      the example programs, you need to copy it and paste it into the
#      Editor tab (the button "Copy example to editor" won't work).

#  (3) Download or build the executable compiled for your platform.
#      The app will run faster, it has better ability to save files,
#      and it doesn't need access to the Internet.

#-------------------------------------------------------------------------------
# Notes on workflow for source
#-------------------------------------------------------------------------------

# Local git source repository
#   Sigma/current/Sigma16
# Online source repository
#   https://github.com/jtod/Sigma16
# Online executable location
#   https://jtod.github.io/S16
# Location on Glasgow web server
#   jtod@sibu:/users/staff/jtod/public_html/Sigma16/

# The primary repository for the source code is kept on my local
# machine and github under the project Sigma16.  To make changes to
# the source

# Edit source files in git repository
# git status
# git add (files that have been changed)
# git commit 'm "purpose of these changes"

# To change version number

# To advance version number to to v3.0.26 or whatever it is...
# edit package.json     This contains the master definition of version number
# make set-version      Reads package.json and defines two auxiliary files
# git tag -a v3.0.26 -m 'move to version v3.0.26'

# To upload new release

# git push origin master

# On github, make a pull request and merge

#-------------------------------------------------------------------------------
# Workflow for online executable version
#-------------------------------------------------------------------------------

# The app won't run directly in the github page: it will show the
# source code but won't render it.  Therefore, to run Sigma16 from the
# web, you need to use the homepage jtod.github.io/S16

# (1) Build the program and upload to project page (as above)

# (2) Copy the source files from the local project repository
#     current/Sigma16 to the version on my homepage, which is
#     current/homeepage/jtod.github.io/S16/releases.  Need to copy the
#     relevant files from the git repository for Sigma16 over to the
#     git repository for jtod.github.io.  Don't copy all files, as
#     these will include library files downloaded by npm for compiling
#     the system.  make copy-app-to-homepage

# (3) Edit homepage/jtod.github.io/S16/index.md to point to the latest
#     release.

# (4) Commit on the releases branch, push, go to github.jtod.io, do a
#     pull request and merge.

#     git push origin release   Try this...
#     git push origin v3.0.27   Try this...


# make compile --- Build everything from just the source.  When publishing
# a release, an option would be to build the documentation (user guide
# and the indexes) so the user wouldn't need to have pandoc installed.

.PHONY : compile
compile :
	make devversion
	make dependencies
	make executable
	make move-executable

#-------------------------------------------------------------------------------
# Version
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
	  Copyright (c) $(YEAR) John T. O'Donnell" \
	  > VERSION.txt
	echo "export const s16version = \"$(VERSION)\";" > src/base/version.mjs

#-------------------------------------------------------------------------------
# Generate index pages for examples
#-------------------------------------------------------------------------------

src/datafiles/welcome.html : src/datafiles/srcwelcome.md
	sed "s/VERSION/${VERSION}, ${MONTHYEAR}/g" \
	  src/datafiles/srcwelcome.md > src/datafiles/welcomeTEMP.md
	pandoc --standalone \
          --template=docs/src/userguide/userguide-template.html \
          --variable=css:../../docs/src/userguide/userguidestyle.css \
          -o src/datafiles/welcome.html \
	  src/datafiles/welcomeTEMP.md


#-------------------------------------------------------------------------------
# The following may be out of date following the revision using express...
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Running as standalone program on local machine with npm
#-------------------------------------------------------------------------------

# Download and install npm
#   https://electronjs.org/docss/tutorial/installation
#   install npm from the electronjs.org web page
# In order to compile a native executable, electron is also needed
#   npm install electron --save-dev       ------ install electron

# Run Sigma16 as a standalone app (independent of a browser)
#   npm install                           ------ download dependencies
#   npm start                             ------ run on local machine

# Compile an executable which will run as standalone app
#   npm run mkdist                        ------ build executable


.PHONY : dependencies
dependencies :
	cd app; npm install

# make run -- run the program on the local computer, without using a
# web page from the Internet.

.PHONY : run
run :
	cd app; npm start


# make executable -- use electron-builder to generate a native
# executable for the current platform.  This allows the program to be
# launched by clicking the executable, and it isn't necessary to have
# npm or the other software tools installed.

.PHONY : executable
executable :
	cd app; npm run mkdist

# make move-exe --- move the executable from dist directory into
# release directory.  There is a bug in Electron-builder: it gives a
# bad name to the exe file; for example it produces 'sigma16
# 3.0.1-7.2.exe' including the quote characters, and if a better name
# is specified using artifactName it fails to expand the variables.
# So in building the release, the executable files are renamed as they
# are moved.

.PHONY : move-executable
move-executable :
	mv src/gui/dist/*.exe release/Sigma16-$(VERSION)-win.exe

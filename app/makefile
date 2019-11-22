#-------------------------------------------------------------------------------
# makefile for Sigma16
# Sigma16 is free software, see LICENSE and README
# Copyright (c) 2019 John O'Donnell
#-------------------------------------------------------------------------------

# This makefile documents how to maintain, build, and run the system

#-------------------------------------------------------------------------------
# Define parameters
#-------------------------------------------------------------------------------

# File locations on development machine

# SIGMACURRENT contains several related projects, including Sigma16

SIGMACURRENT:=../..

# S16WEBPAGE is a directory in my homepage on github; this is where
# the web release is placed, since users can run the app by clicking a
# link pointing into this area

S16WEBPAGE:=$(SIGMACURRENT)/homepage/jtod.github.io/S16

# Extract the version from the package.json file; it's on the line
# consisting of "version: : "1.2.3".  This defines VERSION, which is
# used for building the top level index and the user guide.

VERSION:=$(shell cat package.json | grep version | head -1 | awk -F= "{ print $2 }" | sed 's/[version:,\",]//g' | tr -d '[[:space:]]')

showparams :
	echo SIGMACURRENT = $(SIGMACURRENT)
	echo S16WEBPAGE = $(S16WEBPAGE)
	echo VERSION = $(VERSION)

#-------------------------------------------------------------------------------
# Usage
#-------------------------------------------------------------------------------

# Build the app
#   make web-release        build web page for posting on Internet
#   make compile            build executable using npm to compile from source

# Needed to build both web and compiled version
#   make set-version        get version number from package.json
#   make top-index          generate html from markdown source
#   make userguide          generate html from markdown source
#   make program-indices    index for each directory in programs and examples

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
#  doc/html            written by make doc

#-------------------------------------------------------------------------------
# Make a release for posting on the web
#-------------------------------------------------------------------------------

#  1.  make release
#  2.  cd ../../homepage/jtod.github.io/S16
#  3.  edit README.md to point to the new release
#  4.  git push origin release

# make release -- create a directory containing the source release of
# the current version.  The app can be launched by clicking a link,
# without needing to download anything.

.PHONY : web-release
web-release :
	make set-version
	make top-index
	make userguide
	make program-indices
	mkdir -p ../release/$(VERSION)
	cp ../VERSION ../release/$(VERSION)
	cp ../LICENSE.txt ../release/$(VERSION)
	cp ../index.html ../release/$(VERSION)
	mkdir -p ../release/$(VERSION)/app
	cp -r datafiles ../release/$(VERSION)/app
	cp -r doc ../release/$(VERSION)/app
	cp -r programs ../release/$(VERSION)/app
	cp *.html ../release/$(VERSION)/app
	cp *.css ../release/$(VERSION)/app
	cp *.js ../release/$(VERSION)/app
	cp *.json ../release/$(VERSION)/app
	cp -r  ../release/$(VERSION) $(S16WEBPAGE)/releases

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
# make userguide        Reads an auxiliary file to include version nmber
# git status
# git add (files that have been changed)
# git commit 'm "purpose of these changes"

# To change version number

# To advance version number to to v3.0.26 or whatever it is...
# edit package.json     This contains the master definition of version number
# make set-version      Reads package.json and defines two auxiliary files
# make userguide        Need to update version number in the guide (using aux file)
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
	make set-version
	make userguide
	make top-index
	make dependencies
	make release
	make executable
	make move-executable

# make set-version --- The version number is defined in
# app/package.json; this makefile finds the number there and defines a
# make variable $(VERSION).  This is used in several places, including
# writing a VERSION file in the top directory (not essential but
# helpful for users) and app/version.js (which makes the version
# number available to the JavaScript program).

.PHONY : set-version
set-version :
	echo $(VERSION) > ../VERSION
	echo "const s16version = \"$(VERSION)\";" > version.js


# make program-indices --- Generate the index.html files for the
# programs directory

.PHONY : program-indices
program-indices :
	pandoc --standalone \
          --template=doc/src/programindex-template.html \
          --variable=css:../../doc/src/doc.css \
	  --metadata pagetitle="Example programs" \
          -o programs/Examples/index.html \
	  programs/Examples/index.md
	pandoc --standalone \
          --template=doc/src/programindex-template.html \
          --variable=css:../../../doc/src/doc.css \
	  --metadata pagetitle="Arithmetic examples" \
          -o programs/Examples/Arithmetic/index.html \
	  programs/Examples/Arithmetic/index.md
	pandoc --standalone \
          --template=doc/src/programindex-template.html \
          --variable=css:../../../doc/src/doc.css \
	  --metadata pagetitle="Simple examples" \
          -o programs/Examples/Simple/index.html \
	  programs/Examples/Simple/index.md
	pandoc --standalone \
          --template=doc/src/programindex-template.html \
          --variable=css:../../../doc/src/doc.css \
	  --metadata pagetitle="Array examples" \
          -o programs/Examples/Arrays/index.html \
	  programs/Examples/Arrays/index.md
	pandoc --standalone \
          --template=doc/src/programindex-template.html \
          --variable=css:../../../doc/src/doc.css \
	  --metadata pagetitle="Data structures" \
          -o programs/Examples/DataStructures/index.html \
	  programs/Examples/DataStructures/index.md
	pandoc --standalone \
          --template=doc/src/programindex-template.html \
          --variable=css:../../../doc/src/doc.css \
	  --metadata pagetitle="Input/Output" \
          -o programs/Examples/IO/index.html \
	  programs/Examples/IO/index.md
	pandoc --standalone \
          --template=doc/src/programindex-template.html \
          --variable=css:../../../doc/src/doc.css \
	  --metadata pagetitle="Recursion" \
          -o programs/Examples/Recursion/index.html \
	  programs/Examples/Recursion/index.md
	pandoc --standalone \
          --template=doc/src/programindex-template.html \
          --variable=css:../../../doc/src/doc.css \
	  --metadata pagetitle="Sorting examples" \
          -o programs/Examples/Sorting/index.html \
	  programs/Examples/Sorting/index.md
	pandoc --standalone \
          --template=doc/src/programindex-template.html \
          --variable=css:../../../doc/src/doc.css \
	  --metadata pagetitle="Subroutines" \
          -o programs/Examples/Subroutines/index.html \
	  programs/Examples/Subroutines/index.md
	pandoc --standalone \
          --template=doc/src/programindex-template.html \
          --variable=css:../../../doc/src/doc.css \
	  --metadata pagetitle="Testing" \
          -o programs/Examples/Testing/index.html \
	  programs/Examples/Testing/index.md
	pandoc --standalone \
          --template=doc/src/programindex-template.html \
          --variable=css:../../../doc/src/doc.css \
	  --metadata pagetitle="Type conversion" \
          -o programs/Examples/TypeConversion/index.html \
	  programs/Examples/TypeConversion/index.md

# make userguide --- Generate the user guide html file from markdown
# source

.PHONY : userguide
userguide :
	mkdir -p doc/html
	cp -r doc/src/figures doc/html
	cp doc/src/doc.css doc/html
	pandoc --standalone \
          --template=doc/src/userguide-template.html \
          --table-of-contents --toc-depth=3 \
          --variable=version:'$(VERSION)' \
          --variable=date:'$(VersionDate)' \
          --variable=css:doc.css \
          -o doc/html/userguide-index.html \
	  doc/src/userguide-index.md

# make top-index --- Generate index for the project from markdown
# source.

.PHONY : top-index
top-index :
	pandoc --standalone \
          --template=doc/src/readme-template.html \
          --variable=version:'$(VERSION)' \
          --variable=css:'./app/doc/src/doc.css' \
          --metadata pagetitle='Sigma16 ${VERSION}' \
	  -o ../index.html ../README.md

#-------------------------------------------------------------------------------
# make npm -- download JavaScript dependencies
#-------------------------------------------------------------------------------

.PHONY : dependencies
dependencies :
	npm install

# make run -- run the program on the local computer, without using a
# web page from the Internet.

.PHONY : run
run :
	npm start



# make executable -- use electron-builder to generate a native
# executable for the current platform.  This allows the program to be
# launched by clicking the executable, and it isn't necessary to have
# npm or the other software tools installed.

.PHONY : executable
executable :
	npm run mkdist

# make move-exe --- move the executable from dist directory into
# release directory.  There is a bug in Electron-builder: it gives a
# bad name to the exe file; for example it produces 'sigma16
# 3.0.1-7.2.exe' including the quote characters, and if a better name
# is specified using artifactName it fails to expand the variables.
# So in building the release, the executable files are renamed as they
# are moved.

.PHONY : move-executable
move-executable :
	mv dist/*.exe ../release/Sigma16-$(VERSION)-win.exe

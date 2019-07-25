# makefile for Sigma16

# Non-source files.  The following files are written by various
# compilation tools; they are not source and shouldn't be edited.  The
# files marked optional are produced when a standalone version is
# generated, but are not necessaryand can be deleted.

#  dist                directory produced by npm (optional)
#  node_modules        directory of packages downloaed by npm (optional)
#  package-lock.json   records package versions; produced by npm (optional)
#  version.js          written by make set-version
#  doc/html            written by make doc


# make all                  build everything from source
# make set-version          get version number from package.json
# make userguide            generate html from markdown source
# make top-index            generate html from markdown source
# make release-index        generate html from markdown source
# make dependencies         use npm to download Javascript dependencies
# make run                  run locally
# make release              make a directory for publication
# make executable           package up the code into a native executable
# make move-exe             move the executable into release
# make clean                remove temp and backup files
# make very-clean           remove most generated files
# make super-clean          remove all generated files, keeping only sources
# make show-parameters      print the makefile variables
# make archive-tarball      save a .tgz tarball in archive directory
# make list-archive         list contents of the archive directory

#---------------------------------------------------------------------
# Calculations: don't need to edit these

# Extract the version from the package.json file; it's on the line
# consisting of "version: : "1.2.3",

VERSION:=$(shell cat package.json | grep version | head -1 | awk -F= "{ print $2 }" | sed 's/[version:,\",]//g' | tr -d '[[:space:]]')

# The ArchiveLocation is relative to Sigma16/app
ArchiveLocation := ../../../archive

PackagePath := $(shell pwd)
PackageName := Sigma16
DateTimeStamp := $(shell date +%Y-%m-%d-at-%H-%M)
BuildDate != date +%d\ %b\ %Y
TarballName := $(PackageName)-on-$(DateTimeStamp).tgz
FullCopyName := $(PackageName)-on-$(DateTimeStamp)

# make all --- Build everything from just the source.  When publishing
# a release, an option would be to build the documentation (user guide
# and the indexes) so the user wouldn't need to have pandoc installed.

.PHONY : all
all :
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
          --template=doc/src/top-index-template.html \
          --variable=version:'$(VERSION)' \
          --variable=css:'./app/doc/src/doc.css' \
          -o ../index.html \
	  doc/src/top-index.md
	cp doc/src/doc.css ../

# make release-index --- The index.html for the release directory is
# slightly different from the top level index.  It's generated from
# markdown source.

.PHONY : release-index
release-index :
	mkdir -p ../release
	pandoc --standalone \
          --template=doc/src/release-index-template.html \
          --variable=version:'$(VERSION)' \
          --variable=date:'this is version date $(VersionDate)' \
          --variable=css:'./doc.css' \
          -o ../release/index.html \
	  doc/src/release-index.md
	cp doc/src/doc.css ../release/

# make npm -- download JavaScript dependencies

.PHONY : dependencies
dependencies :
	npm install

# make run -- run the program on the local computer, without using a
# web page from the Internet.

.PHONY : run
run :
	npm start

# make release -- create a directory containing the full release of
# the current version.  To publish the release, move the directory to
# the project web page.  This builds everything in the release
# directory apart from the executable; use make move-exe to get that
# as well.

.PHONY : release
release :
	mkdir -p ../release/Sigma16-$(VERSION)
	make set-version
	cp ../VERSION ../release/Sigma16-$(VERSION)
	cp ../LICENSE.txt ../release/Sigma16-$(VERSION)
	cp ../README.md ../release/Sigma16-$(VERSION)
	mkdir -p ../release/Sigma16-$(VERSION)/app
	cp makefile ../release/Sigma16-$(VERSION)/app
	cp *.html ../release/Sigma16-$(VERSION)/app
	cp *.css ../release/Sigma16-$(VERSION)/app
	cp *.js ../release/Sigma16-$(VERSION)/app
	cp *.json ../release/Sigma16-$(VERSION)/app
	cp -r datafiles ../release/Sigma16-$(VERSION)/app
	cp -r doc ../release/Sigma16-$(VERSION)/app
	cp -r programs ../release/Sigma16-$(VERSION)/app
	zip -r ../release/Sigma16-$(VERSION).zip ../release/Sigma16-$(VERSION)
	make release-index

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
	mv dist/*.exe ../release/Sigma16-Sigma16-$(VERSION)-win.exe

# make clean -- Remove backup and object files, and temporary
# compilation files.  Leaves executable, documentation, and source in
# place.

.PHONY : clean
clean :
	cd ..; find . \( -name '*~' -o -name '*.bak' \) -delete

# make very-clean -- remove html documentation files generated by
# panddoc, leaving a minimal source directory

.PHONY : very-clean
very-clean :
	make clean
	rm -rf ../release
	rm -f ../VERSION
	rm -rf dist

# make super-clean --- also removes the html documentation and the
# node_modules downloaded by npm

.PHONY : super-clean
super-clean :
	make very-clean
	rm -rf doc/html
	rm -rf node_modules
	rm ../index.html

# make show-parameters -- print calculated parameters, for maintaining
# this makefile

.PHONY : show-parameters
show-parameters :
	echo VERSION = $(VERSION)
	echo PackagePath = $(PackagePath)
	echo PackageName = $(PackageName)
	echo DateTimeStamp = $(DateTimeStamp)
	echo BuildDate = $(BuildDate)
	echo TarballName = $(TarballName)
	echo FullCopyName = $(FullCopyName)
	echo ArchiveLocation = $(ArchiveLocation)
	echo Sigma16Version = $(Sigma16Version)

# make archive-tarball -- create a tgz file of the entire
# Sigma16-i.j.k directory and place it in the archive directory.  See
# README.txt for directory layout.

.PHONY : archive-tarball
archive-tarball :
	cd ../.. ; \
	  pwd ; \
	  tar -czf $(TarballName) $(PackageName) ; \
	  mv $(TarballName) $(ArchiveLocation)

# make list-archive -- list contents of the archive directory
.PHONY : list-archive
list-archive :
	ls -lctr $(ArchiveLocation)

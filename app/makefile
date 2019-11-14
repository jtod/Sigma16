# makefile for Sigma16


# edit package.json to change version number
# make set-version
# make userguide
# git tag -a v3.0.26 -m 'move to version v3.0.26'
# git push  origin v3.0.26

# Does github automatically build the web page?  Do I need to touch it
# on settings?

# On github project page, select current tag, go to settings, select
# master (evern though it is already selected)


# On gitghb project page, https://github.com/jtod/Sigma16/settings,
# this message appears (soon after pushing): "Your GitHub Pages site
# is currently being built from the master branch. Learn more."

# make web     prepare web version


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

RELEASELOCATION := jtod@sibu:/users/staff/jtod/public_html/Sigma16/

#---------------------------------------------------------------------
# make web - make the files needed for running as a web page

.PHONY : web
web :
	make set-version
	make userguide
	make top-index

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
	rm -rf ../release/*
	mkdir -p ../release
	pandoc --standalone \
          --template=doc/src/release-index-template.html \
          --variable=version:'$(VERSION)' \
          --variable=date:'this is version date $(VersionDate)' \
          --variable=css:'./doc.css' \
          -o ../release/index.html \
	  doc/src/release-index.md
	cp doc/src/doc.css ../release/

#---------------------------------------------------------------------
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
	rm -rf ../release/*
	mkdir -p ../release/$(VERSION)
	make set-version
	cp ../VERSION ../release/$(VERSION)
	cp ../LICENSE.txt ../release/$(VERSION)
	cp ../README.md ../release/$(VERSION)
	mkdir -p ../release/$(VERSION)/app
	cp makefile ../release/$(VERSION)/app
	cp *.html ../release/$(VERSION)/app
	cp *.css ../release/$(VERSION)/app
	cp *.js ../release/$(VERSION)/app
	cp *.json ../release/$(VERSION)/app
	cp -r datafiles ../release/$(VERSION)/app
	cp -r doc ../release/$(VERSION)/app
	cp -r programs ../release/$(VERSION)/app
	zip -r ../release/$(VERSION).zip ../release/$(VERSION)
	pandoc --standalone \
          --template=doc/src/release-index-template.html \
          --variable=version:'$(VERSION)' \
          --variable=date:'this is version date $(VersionDate)' \
          --variable=css:'./doc.css' \
          -o ../release/$(VERSION)/index.html \
	  doc/src/release-index.md
	cp doc/src/doc.css ../release/$(VERSION)

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

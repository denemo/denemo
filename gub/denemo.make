# -*-Makefile-*-
.PHONY: all default packages rest update-versions print-success print-branches
.PHONY: nsis denemo denemo-installers
default: all

#DENEMO_BRANCH="stable-0.9.0"
DENEMO_BRANCH="master"
DENEMO_REPO_URL=git://git.savannah.gnu.org/denemo.git

PLATFORMS=mingw

# derived info
DENEMO_SOURCE_URL=$(DENEMO_REPO_URL)?branch=$(DENEMO_BRANCH)
DENEMO_DIRRED_BRANCH=$(shell $(PYTHON) gub/repository.py --branch-dir '$(DENEMO_SOURCE_URL)')
DENEMO_FLATTENED_BRANCH=$(shell $(PYTHON) gub/repository.py --full-branch-name '$(DENEMO_SOURCE_URL)')
# FOR BUILDING from GIT
#BUILD_PACKAGE='$(DENEMO_SOURCE_URL)'
BUILD_PACKAGE=denemo
INSTALL_PACKAGE = denemo

MAKE += -f denemo.make

# urg, from lilypond.make -- should share lilypond info
#LILYPOND_BRANCH=master
LILYPOND_REPO_URL=git://git.sv.gnu.org/lilypond.git
# derived info
LILYPOND_SOURCE_URL=$(LILYPOND_REPO_URL)?branch=$(LILYPOND_BRANCH)
LILYPOND_DIRRED_BRANCH=$(shell $(PYTHON) gub/repository.py --branch-dir '$(LILYPOND_SOURCE_URL)')
LILYPOND_FLATTENED_BRANCH=$(shell $(PYTHON) gub/repository.py --full-branch-name '$(LILYPOND_SOURCE_URL)')

# FOR BUILDING from GIT
INSTALLER_BUILDER_OPTIONS =\
 --version-db=versiondb/denemo.versions\
 $(if $(DENEMO_BRANCH), --branch=denemo=$(DENEMO_FLATTENED_BRANCH),)\
 $(if $(LILYPOND_BRANCH), --branch=lilypond=$(LILYPOND_FLATTENED_BRANCH),)\
#

include gub.make
include compilers.make

#all: packages rest
all: denemo rest
ifeq ($(findstring mingw, $(PLATFORMS)),mingw)
rest: nsis
endif
rest: denemo-installers print-success

#avoid building native BUILD_PLATFORM
denemo:
	$(foreach p, $(PLATFORMS), $(call INVOKE_GUB,$(p)) $(BUILD_PACKAGE) && ) true #

denemo-installers:
	$(foreach p, $(PLATFORMS), $(call INVOKE_INSTALLER_BUILDER,$(p)) $(INSTALL_PACKAGE) &&) true #

nsis:
	bin/gub tools::nsis

update-versions:
	python gub/versiondb.py --no-sources --version-db=versiondb/denemo.versions --download --platforms="mingw" --url=http://lilypond.org/blog/janneke/software/denemo

print-success:
	@echo "success!!"
	@echo Denemo installer in uploads/denemo*.mingw.exe

print-branches:
	@echo "--branch=guile=$(GUILE_FLATTENED_BRANCH)"
	@echo "--branch=lilypond=$(LILYPOND_FLATTENED_BRANCH)"
	@echo "--branch=denemo=$(DENEMO_FLATTENED_BRANCH)"

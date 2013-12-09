#!/bin/bash
set -e

gcc_dependencies()
{
  sudo apt-get -qq install intltool guile-2.0-dev libsndfile-dev libsmf-dev libgtk-3-dev libgtksourceview-3.0-dev libevince-dev libfluidsynth-dev librubberband-dev portaudio19-dev libaubio-dev libfftw3-dev libportmidi-dev gtk-doc-tools lilypond
}

if [ "$COMPILER"x = "gcc"x ]; then
  gcc_dependencies
elif [ "$COMPILER"x = "clang"x ]; then
  gcc_dependencies
fi

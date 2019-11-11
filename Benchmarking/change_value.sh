#!/usr/bin/env bash

OLD_VALUES=(10 30 50 70 90 110)
NEW_VALUES=(10 20 30 40 50 60)

for i in {0..5}; do
  REPLACEMENT=s/${OLD_VALUES[$i]}/${NEW_VALUES[$i]}/g

  sed $REPLACEMENT -i ./Extern/Insert/Insert${OLD_VALUES[$i]}.hs
  mv ./Extern/Insert/Insert${OLD_VALUES[$i]}.hs ./Extern/Insert/Insert${NEW_VALUES[$i]}.hs
  sed $REPLACEMENT -i ./Extern/Delete/Delete${OLD_VALUES[$i]}.hs
  mv ./Extern/Delete/Delete${OLD_VALUES[$i]}.hs ./Extern/Delete/Delete${NEW_VALUES[$i]}.hs
  sed $REPLACEMENT -i ./Extern/Lookup/Lookup${OLD_VALUES[$i]}.hs
  mv ./Extern/Lookup/Lookup${OLD_VALUES[$i]}.hs ./Extern/Lookup/Lookup${NEW_VALUES[$i]}.hs

  sed $REPLACEMENT -i ./FullExtern/Insert/Insert${OLD_VALUES[$i]}.hs
  mv ./FullExtern/Insert/Insert${OLD_VALUES[$i]}.hs ./FullExtern/Insert/Insert${NEW_VALUES[$i]}.hs
  sed $REPLACEMENT -i ./FullExtern/Delete/Delete${OLD_VALUES[$i]}.hs
  mv ./FullExtern/Delete/Delete${OLD_VALUES[$i]}.hs ./FullExtern/Delete/Delete${NEW_VALUES[$i]}.hs
  sed $REPLACEMENT -i ./FullExtern/Lookup/Lookup${OLD_VALUES[$i]}.hs
  mv ./FullExtern/Lookup/Lookup${OLD_VALUES[$i]}.hs ./FullExtern/Lookup/Lookup${NEW_VALUES[$i]}.hs

  sed $REPLACEMENT -i ./Intern/Insert/Insert${OLD_VALUES[$i]}.hs
  mv ./Intern/Insert/Insert${OLD_VALUES[$i]}.hs ./Intern/Insert/Insert${NEW_VALUES[$i]}.hs
  sed $REPLACEMENT -i ./Intern/Delete/Delete${OLD_VALUES[$i]}.hs
  mv ./Intern/Delete/Delete${OLD_VALUES[$i]}.hs ./Intern/Delete/Delete${NEW_VALUES[$i]}.hs
  sed $REPLACEMENT -i ./Intern/Lookup/Lookup${OLD_VALUES[$i]}.hs
  mv ./Intern/Lookup/Lookup${OLD_VALUES[$i]}.hs ./Intern/Lookup/Lookup${NEW_VALUES[$i]}.hs
done

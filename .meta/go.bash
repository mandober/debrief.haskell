#!/bin/bash

basedir="/mnt/t/code/git-debrief/debrief.haskell"

indir="${basedir}/extensions/guide"
inext="*.rst"

outdir="${basedir}/extensions/guiderst"

FILES="${basedir}/${indir}/${inext}"

cd $indir
for f in $inext; do
  fname="${f%.*}"
  echo "Converting $f to $fname.md"
  pandoc $f --from=rst --to=markdown --output="./${fname}.md"
done

# pandoc "$f" --from=rst --to=markdown --output="$fname.md"
# pandoc fname.rst -f rst -t markdown -o fname.md
# pandoc fname.rst --from=rst --to=markdown --output=fname.md

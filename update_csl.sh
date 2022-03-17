#!/bin/sh

# This script is adaptation of Brenton Wiernik's script at retrieved from
# https://github.com/bwiernik/zotero-tools

cd inst/rmd

# Remove previous versions
for i in {6..7}
do
  rm -f apa$i.csl apa$i-no-disambiguation.csl apa$i-annotated.csl apa$i-annotated-no-disambiguation.csl
done

# Download latest CSL files
curl -sS "https://raw.githubusercontent.com/citation-style-language/styles/master/apa-6th-edition.csl" --output "apa6.csl"
curl -sS "https://raw.githubusercontent.com/citation-style-language/styles/master/apa.csl" --output "apa7.csl"

# Create variants
for i in {6..7}
do
  sed -e 's/<layout>/<layout>\
      <text variable="annote" vertical-align="sup" suffix=" "\/>/' -e "s/American Psychological Association ${i}th edition/American Psychological Association ${i}th edition (annotated)/g" -e 's/styles\/apa/styles\/apa-annotated/g' -e 's/rel="self"\/>/rel="self"\/>\
      <link href="http:\/\/www.zotero.org\/styles\/apa" rel="template"\/>/g'<apa$i.csl >apa$i-annotated.csl
  for j in '' '-annotated'
  do sed -e 's/disambiguate-add-givenname="true"//g' -e 's/disambiguate-add-givenname="true"//g' -e 's/givenname-disambiguation-rule="primary-name"//g' -e "s/American Psychological Association 6th edition\( *(*\)\([^\)<]*\))*/American Psychological Association ${i}th edition (\2without given name disambiguation)/g" -e "s/(annotatedwithout/(annotated without/g" -e "s/styles\/apa/styles\/apa${j}-no-disambiguation/g" -e 's/rel="self"\/>/rel="self"\/>\n    <link href="http:\/\/www.zotero.org\/styles\/apa" rel="template"\/>/g'<apa$i$j.csl >apa$i$j-no-disambiguation.csl
  done
done

cd ../..

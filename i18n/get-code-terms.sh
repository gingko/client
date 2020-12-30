#!/usr/bin/env bash

# Translation.elm to all-language .po
sed -E '
s/\s{16}(\S*) ->/#:Elm:\1/
s/\s{16}(\S*) n ->/#:Elm:\1/
s/\s{20}\{ en = "(.*)"/msgid "\1"\nmsgstr ""/
s/\s{20}\{ en = .*"(.*)" "(.*)"/msgid "\1"\nmsgid_plural "\2"\nmsgstr[0] ""\nmsgstr[1] ""/
/(^#:|msgid|msgstr)/!d
s/^#:(.*)/\n#:\1/
' ../src/elm/Translation.elm > elm.po

# translation.js to all-language .po
sed -E '
s/\s{4}(\S*) :/#:JS:\1/
s/\s{2}, (\S*) :/#:JS:\1/
s/\s{6}\{ en : "(.*)"/msgid "\1"\nmsgstr ""/
/(^#:|msgid|msgstr)/!d
s/^#:(.*)/\n#:\1/
' ../src/shared/translation.js > js.po

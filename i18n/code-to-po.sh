#!/usr/bin/env bash

# Translation.elm to es-elm.po
sed -E '
/(en|es) = ".*"/!d
s/^.*en = (".*").*$/msgid \1/
s/^.*es = (".*").*$/msgstr \1\n/
' ../src/elm/Translation.elm > es-elm.po

# translation.js to es-js.po
sed -E '
/(en|es) : /!d
s/^.*en : (.*)$/msgid \1/
s/^.*es : (.*)$/msgstr \1\n/
' ../src/shared/translation.js > es-js.po


printf 'msgid ""\nmsgstr ""\n"Plural-Forms: nplurals=2; plural=n != 1;\\n"\n\n' | cat - es-elm.po es-js.po > es.po


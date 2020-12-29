#!/usr/bin/env bash


# ========== ZH : Chinese ===========
# Translation.elm to zh-elm.po
sed -E '
/(en|zh) = ".*"/!d
s/^.*en = (".*").*$/msgid \1/
s/^.*zh = (".*").*$/msgstr \1\n/
' ../src/elm/Translation.elm > zh-elm.po

# translation.js to zh-js.po
sed -E '
s/^\s{2}, (\S*) :.*$/#: \1/
s/^\s{4}(\S*) :.*$/#: \1/
s/^.*en : (.*)$/msgid \1/
s/^.*zh : (.*)$/msgstr \1\n/
/(^#:|^msg)/!d
' ../src/shared/translation.js > zh-js.po

# Merge files
printf 'msgid ""\nmsgstr ""\n"MIME-Version: 1.0\\n"\n"Content-Type: text/plain; charset=UTF-8\\n"\n"Content-Transfer-Encoding: 8bit\\n"\n"X-Generator: POEditor.com\\n"\n"Project-Id-Version: Gingko Desktop\\n"\n"Language: zh-Hans\\n"\n"Plural-Forms: nplurals=1; plural=0;\\n"\n\n' | cat - zh-elm.po zh-js.po > zh.po

# Delete temp files
rm zh-*.po




# ========== ES : Spanish ===========
# Translation.elm to es-elm.po
sed -E '
/(en|es) = ".*"/!d
s/^.*en = (".*").*$/msgid \1/
s/^.*es = (".*").*$/msgstr \1\n/
' ../src/elm/Translation.elm > es-elm.po

# translation.js to es-js.po
sed -E '
s/^\s{2}, (\S*) :.*$/#: \1/
s/^\s{4}(\S*) :.*$/#: \1/
s/^.*en : (.*)$/msgid \1/
s/^.*es : (.*)$/msgstr \1\n/
/(^#:|^msg)/!d
' ../src/shared/translation.js > es-js.po

# Merge files
printf 'msgid ""\nmsgstr ""\n"Plural-Forms: nplurals=2; plural=n != 1;\\n"\n\n' | cat - es-elm.po es-js.po > es.po

# Delete temp files
rm es-*.po




# ========== FR : French ===========
# Translation.elm to fr-elm.po
sed -E '
/(en|fr) = ".*"/!d
s/^.*en = (".*").*$/msgid \1/
s/^.*fr = (".*").*$/msgstr \1\n/
' ../src/elm/Translation.elm > fr-elm.po

# translation.js to fr-js.po
sed -E '
s/^\s{2}, (\S*) :.*$/#: \1/
s/^\s{4}(\S*) :.*$/#: \1/
s/^.*en : (.*)$/msgid \1/
s/^.*fr : (.*)$/msgstr \1\n/
/(^#:|^msg)/!d
' ../src/shared/translation.js > fr-js.po

# Merge files
printf 'msgid ""\nmsgstr ""\n"Plural-Forms: nplurals=2; plural=(n > 1)\\n"\n\n' | cat - fr-elm.po fr-js.po > fr.po

# Delete temp files
rm fr-*.po




# ========== SV : Swedish ===========
# Translation.elm to sv-elm.po
sed -E '
/(en|sv) = ".*"/!d
s/^.*en = (".*").*$/msgid \1/
s/^.*sv = (".*").*$/msgstr \1\n/
' ../src/elm/Translation.elm > sv-elm.po

# translation.js to sv-js.po
sed -E '
s/^\s{2}, (\S*) :.*$/#: \1/
s/^\s{4}(\S*) :.*$/#: \1/
s/^.*en : (.*)$/msgid \1/
s/^.*sv : (.*)$/msgstr \1\n/
/(^#:|^msg)/!d
' ../src/shared/translation.js > sv-js.po

# Merge files
printf 'msgid ""\nmsgstr ""\n"Plural-Forms: nplurals=2; plural=(n > 1)\\n"\n\n' | cat - sv-elm.po sv-js.po > sv.po

# Delete temp files
rm sv-*.po

import { readFileSync } from "fs";
import { fileURLToPath } from "url";
import { dirname, join } from "path";
import config from "./config.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const languages = [
  "zh_hans", "zh_hant", "es", "ar", "fr", "ru", "de", "ja", "mr", "pes",
  "it", "ro", "hr", "nl", "hu", "sv", "ca", "br", "cs", "fa",
  "id", "ko", "pl", "uk", "nb",
];

const prepTranslation = (langCode, langData) => {
  return langData.flatMap(t => {
    let target = t.reference.replace('Elm:', `%${langCode}:`) + "%";
    let replacement = t.definition;
    if (typeof target === "string" && target.startsWith(`%${langCode}`) && typeof replacement === "string" ) {
      // Regular replacement
      return [{ search: target
        , replace: replacement.replace('\n', '\\n').replace(/'/g,"\\'")
      }];
    } else if (replacement === null) {
      return [{ search: target
        , replace: t.term.replace(/'/g, "\\'")
      }];
    } else if (t.hasOwnProperty("term_plural") && typeof replacement == "object" && replacement.hasOwnProperty("one")) {
      // Plural replacement
      let singReplace = replacement.one.replace(/'/g,"\\'");
      let plurReplace = replacement.other.replace(/'/g, "\\'");
      plurReplace = plurReplace == "" ? t.term_plural : plurReplace;
      return [
        { search: target+":0" , replace: singReplace },
        { search: target+":1" , replace: plurReplace }];
    } else {
      return [];
    }
  });
}

const allLanguageStrings = languages.flatMap(langCode =>
  prepTranslation(langCode, JSON.parse(readFileSync(join(__dirname, `./i18n/${langCode}.json`), "utf-8")))
);

const otherReplacements = [
  { search: '{%SUPPORT_EMAIL%}', replace: config.SUPPORT_EMAIL },
  { search: '{%SUPPORT_URGENT_EMAIL%}', replace: config.SUPPORT_URGENT_EMAIL },
  { search: '{%HOMEPAGE_URL%}', replace: config.HOMEPAGE_URL },
  { search: '{%TESTIMONIAL_URL%}', replace: config.TESTIMONIAL_URL },
  { search: '{%VOX_EMPORIUM_SALT%}', replace: config.VOX_EMPORIUM_SALT },
];

// One scan of the (1.8 MiB) bundle instead of one per replacement: every
// placeholder has a fixed shape, so we match the shape and look the token up.
// First definition wins, like the sequential replaceAll pass it replaces.
const replacements = new Map();
for (const { search, replace } of allLanguageStrings.concat(otherReplacements)) {
  if (typeof replace === "string" && !replacements.has(search)) {
    replacements.set(search, replace);
  }
}

const placeholder = /%[A-Za-z_]+:[A-Za-z0-9_]+%(?::[01])?|\{%[A-Z_]+%\}/g;

/**
 * @type {import("elm-watch/elm-watch-node").Postprocess}
 */
export default function postprocess({ code, compilationMode }) {
  if (compilationMode === 'optimize') {
    code = code.replace(placeholder, (match) => {
      const replacement = replacements.get(match);
      if (replacement !== undefined) return replacement;

      // A `:0`/`:1` plural suffix is only part of the token when a plural
      // definition exists; otherwise the bare token is what gets replaced.
      const suffix = match.slice(-2);
      if (suffix === ":0" || suffix === ":1") {
        const base = replacements.get(match.slice(0, -2));
        if (base !== undefined) return base + suffix;
      }

      return match;
    });
  }

  return code;
}

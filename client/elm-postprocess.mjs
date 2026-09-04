import { readFileSync } from "fs";
import { fileURLToPath } from "url";
import { dirname, join } from "path";
import config from "./config.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Load JSON files using fs.readFileSync for Node 16+ compatibility
const zh_hans = JSON.parse(readFileSync(join(__dirname, "./i18n/zh_hans.json"), "utf-8"));
const zh_hant = JSON.parse(readFileSync(join(__dirname, "./i18n/zh_hant.json"), "utf-8"));
const es = JSON.parse(readFileSync(join(__dirname, "./i18n/es.json"), "utf-8"));
const ar = JSON.parse(readFileSync(join(__dirname, "./i18n/ar.json"), "utf-8"));
const fr = JSON.parse(readFileSync(join(__dirname, "./i18n/fr.json"), "utf-8"));
const ru = JSON.parse(readFileSync(join(__dirname, "./i18n/ru.json"), "utf-8"));
const de = JSON.parse(readFileSync(join(__dirname, "./i18n/de.json"), "utf-8"));
const ja = JSON.parse(readFileSync(join(__dirname, "./i18n/ja.json"), "utf-8"));
const mr = JSON.parse(readFileSync(join(__dirname, "./i18n/mr.json"), "utf-8"));
const pes = JSON.parse(readFileSync(join(__dirname, "./i18n/pes.json"), "utf-8"));
const it = JSON.parse(readFileSync(join(__dirname, "./i18n/it.json"), "utf-8"));
const ro = JSON.parse(readFileSync(join(__dirname, "./i18n/ro.json"), "utf-8"));
const hr = JSON.parse(readFileSync(join(__dirname, "./i18n/hr.json"), "utf-8"));
const nl = JSON.parse(readFileSync(join(__dirname, "./i18n/nl.json"), "utf-8"));
const hu = JSON.parse(readFileSync(join(__dirname, "./i18n/hu.json"), "utf-8"));
const sv = JSON.parse(readFileSync(join(__dirname, "./i18n/sv.json"), "utf-8"));
const ca = JSON.parse(readFileSync(join(__dirname, "./i18n/ca.json"), "utf-8"));
const br = JSON.parse(readFileSync(join(__dirname, "./i18n/br.json"), "utf-8"));
const cs = JSON.parse(readFileSync(join(__dirname, "./i18n/cs.json"), "utf-8"));
const fa = JSON.parse(readFileSync(join(__dirname, "./i18n/fa.json"), "utf-8"));
const id = JSON.parse(readFileSync(join(__dirname, "./i18n/id.json"), "utf-8"));
const ko = JSON.parse(readFileSync(join(__dirname, "./i18n/ko.json"), "utf-8"));
const pl = JSON.parse(readFileSync(join(__dirname, "./i18n/pl.json"), "utf-8"));
const uk = JSON.parse(readFileSync(join(__dirname, "./i18n/uk.json"), "utf-8"));
const nb = JSON.parse(readFileSync(join(__dirname, "./i18n/nb.json"), "utf-8"));

const prepTranslation = (langCode, langData) => {
  return langData.flatMap(t => {
    let target = t.reference.replace('Elm:', `%${langCode}:`) + "%";
    let replacement = t.definition;
    if (typeof target === "string" && target.startsWith(`%${langCode}`) && typeof replacement === "string" ) {
      // Regular replacement
      return [{ search: target
        , replace: replacement.replace('\n', '\\n').replace(/'/g,"\\'")
        , flags : 'g'
      }];
    } else if (replacement === null) {
      return [{ search: target
        , replace: t.term.replace(/'/g, "\\'")
        , flags : 'g'
      }];
    } else if (t.hasOwnProperty("term_plural") && typeof replacement == "object" && replacement.hasOwnProperty("one")) {
      // Plural replacement
      let singReplace = replacement.one.replace(/'/g,"\\'");
      let plurReplace = replacement.other.replace(/'/g, "\\'");
      plurReplace = plurReplace == "" ? t.term_plural : plurReplace;
      return [
        { search: target+":0" , replace: singReplace, flags : 'g' },
        { search: target+":1" , replace: plurReplace, flags:  'g' }];
    } else {
      return [];
    }
  });
}

const zhHansT = prepTranslation("zh_hans", zh_hans);
const zhHantT = prepTranslation("zh_hant", zh_hant);
const esT = prepTranslation("es", es);
const arT = prepTranslation("ar", ar);
const frT = prepTranslation("fr", fr);
const ruT = prepTranslation("ru", ru);
const deT = prepTranslation("de", de);
const jaT = prepTranslation("ja", ja);
const mrT = prepTranslation("mr", mr);
const pesT = prepTranslation("pes", pes);
const itT = prepTranslation("it", it);
const roT = prepTranslation("ro", ro);
const hrT = prepTranslation("hr", hr);
const nlT = prepTranslation("nl", nl);
const huT = prepTranslation("hu", hu);
const svT = prepTranslation("sv", sv);
const caT = prepTranslation("ca", ca);
const brT = prepTranslation("br", br);
const csT = prepTranslation("cs", cs);
const faT = prepTranslation("fa", fa);
const idT = prepTranslation("id", id);
const koT = prepTranslation("ko", ko);
const plT = prepTranslation("pl", pl);
const ukT = prepTranslation("uk", uk);
const nbT = prepTranslation("nb", nb);

const allLanguageStrings = [].concat(zhHansT, zhHantT, esT, arT, frT, ruT, deT, jaT,mrT, pesT, itT, roT, hrT, nlT, huT, svT, caT, brT, csT, faT, idT, koT, plT, ukT, nbT)
const otherReplacements = [
  { search: '{%SUPPORT_EMAIL%}', replace: config.SUPPORT_EMAIL, flags: 'g' },
  { search: '{%SUPPORT_URGENT_EMAIL%}', replace: config.SUPPORT_URGENT_EMAIL, flags: 'g' },
  { search: '{%HOMEPAGE_URL%}', replace: config.HOMEPAGE_URL, flags: 'g' },
  { search: '{%TESTIMONIAL_URL%}', replace: config.TESTIMONIAL_URL, flags: 'g' },
  { search: '{%VOX_EMPORIUM_SALT%}', replace: config.VOX_EMPORIUM_SALT, flags: 'g' },
]

const allReplacements = allLanguageStrings.concat(otherReplacements);

/**
 * @type {import("elm-watch/elm-watch-node").Postprocess}
 */
export default function postprocess({ code, compilationMode }) {
  if (compilationMode === 'optimize') {
    allReplacements.forEach(replacement => {
      code = code.replaceAll(replacement.search, replacement.replace);
    });
  }

  return code;
}

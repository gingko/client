import zh_hans from "./i18n/zh_hans.json" assert { type: "json" };

/**
 * @type {import("elm-watch/elm-watch-node").Postprocess}
 */
export default function postprocess({ code }) {
  return code;
}
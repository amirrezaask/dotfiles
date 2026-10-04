import { createHighlighterCore, type HighlighterCore } from "@shikijs/core";
import { createJavaScriptRegexEngine } from "@shikijs/engine-javascript";
import githubDark from "@shikijs/themes/github-dark";
import githubLight from "@shikijs/themes/github-light";
import bash from "@shikijs/langs/bash";
import css from "@shikijs/langs/css";
import diff from "@shikijs/langs/diff";
import html from "@shikijs/langs/html";
import javascript from "@shikijs/langs/javascript";
import json from "@shikijs/langs/json";
import jsx from "@shikijs/langs/jsx";
import markdown from "@shikijs/langs/markdown";
import python from "@shikijs/langs/python";
import ruby from "@shikijs/langs/ruby";
import rust from "@shikijs/langs/rust";
import shellscript from "@shikijs/langs/shellscript";
import sql from "@shikijs/langs/sql";
import tsx from "@shikijs/langs/tsx";
import typescript from "@shikijs/langs/typescript";
import yaml from "@shikijs/langs/yaml";

export type HighlightTheme = "github-dark" | "github-light";

const themes = [githubDark, githubLight];
const languages = [bash, css, diff, html, javascript, json, jsx, markdown, python, ruby, rust, shellscript, sql, tsx, typescript, yaml];
let highlighter: Promise<HighlighterCore> | undefined;

export async function codeToHtml(code: string, language: string, theme: HighlightTheme) {
  highlighter ??= createHighlighterCore({
    themes,
    langs: languages,
    engine: createJavaScriptRegexEngine(),
  });
  const instance = await highlighter;
  const lang = instance.getLoadedLanguages().includes(language) ? language : "text";
  return instance.codeToHtml(code, { lang, theme, rootStyle: false, tabindex: false });
}

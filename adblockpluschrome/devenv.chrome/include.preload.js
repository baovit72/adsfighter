/******/ (() => { // webpackBootstrap
/******/ 	"use strict";
/******/ 	var __webpack_modules__ = ({

/***/ 173:
/***/ ((__unused_webpack_module, exports) => {

/*
 * This file is part of Adblock Plus <https://adblockplus.org/>,
 * Copyright (C) 2006-present eyeo GmbH
 *
 * Adblock Plus is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * Adblock Plus is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Adblock Plus.  If not, see <http://www.gnu.org/licenses/>.
 */

/** @module */



/**
 * Converts raw text into a regular expression string
 * @param {string} text the string to convert
 * @return {string} regular expression representation of the text
 * @package
 */
exports.textToRegExp = function textToRegExp(text)
{
  return text.replace(/[-/\\^$*+?.()|[\]{}]/g, "\\$&");
};

/**
 * Converts filter text into regular expression string
 * @param {string} text as in Filter()
 * @return {string} regular expression representation of filter text
 * @package
 */
exports.filterToRegExp = function filterToRegExp(text)
{
  // remove multiple wildcards
  text = text.replace(/\*+/g, "*");

  // remove leading wildcard
  if (text[0] == "*")
    text = text.substring(1);

  // remove trailing wildcard
  if (text[text.length - 1] == "*")
    text = text.substring(0, text.length - 1);

  return text
    // remove anchors following separator placeholder
    .replace(/\^\|$/, "^")
    // escape special symbols
    .replace(/\W/g, "\\$&")
    // replace wildcards by .*
    .replace(/\\\*/g, ".*")
    // process separator placeholders (all ANSI characters but alphanumeric
    // characters and _%.-)
    .replace(/\\\^/g, "(?:[\\x00-\\x24\\x26-\\x2C\\x2F\\x3A-\\x40\\x5B-\\x5E\\x60\\x7B-\\x7F]|$)")
    // process extended anchor at expression start
    .replace(/^\\\|\\\|/, "^[\\w\\-]+:\\/+(?!\\/)(?:[^\\/]+\\.)?")
    // process anchor at expression start
    .replace(/^\\\|/, "^")
    // process anchor at expression end
    .replace(/\\\|$/, "$");
};

let splitSelector = exports.splitSelector = function splitSelector(selector)
{
  if (!selector.includes(","))
    return [selector];

  let selectors = [];
  let start = 0;
  let level = 0;
  let sep = "";

  for (let i = 0; i < selector.length; i++)
  {
    let chr = selector[i];

    if (chr == "\\")        // ignore escaped characters
    {
      i++;
    }
    else if (chr == sep)    // don't split within quoted text
    {
      sep = "";             // e.g. [attr=","]
    }
    else if (sep == "")
    {
      if (chr == '"' || chr == "'")
      {
        sep = chr;
      }
      else if (chr == "(")  // don't split between parentheses
      {
        level++;            // e.g. :matches(div,span)
      }
      else if (chr == ")")
      {
        level = Math.max(0, level - 1);
      }
      else if (chr == "," && level == 0)
      {
        selectors.push(selector.substring(start, i));
        start = i + 1;
      }
    }
  }

  selectors.push(selector.substring(start));
  return selectors;
};

function findTargetSelectorIndex(selector)
{
  let index = 0;
  let whitespace = 0;
  let scope = [];

  // Start from the end of the string and go character by character, where each
  // character is a Unicode code point.
  for (let character of [...selector].reverse())
  {
    let currentScope = scope[scope.length - 1];

    if (character == "'" || character == "\"")
    {
      // If we're already within the same type of quote, close the scope;
      // otherwise open a new scope.
      if (currentScope == character)
        scope.pop();
      else
        scope.push(character);
    }
    else if (character == "]" || character == ")")
    {
      // For closing brackets and parentheses, open a new scope only if we're
      // not within a quote. Within quotes these characters should have no
      // meaning.
      if (currentScope != "'" && currentScope != "\"")
        scope.push(character);
    }
    else if (character == "[")
    {
      // If we're already within a bracket, close the scope.
      if (currentScope == "]")
        scope.pop();
    }
    else if (character == "(")
    {
      // If we're already within a parenthesis, close the scope.
      if (currentScope == ")")
        scope.pop();
    }
    else if (!currentScope)
    {
      // At the top level (not within any scope), count the whitespace if we've
      // encountered it. Otherwise if we've hit one of the combinators,
      // terminate here; otherwise if we've hit a non-colon character,
      // terminate here.
      if (/\s/.test(character))
        whitespace++;
      else if ((character == ">" || character == "+" || character == "~") ||
               (whitespace > 0 && character != ":"))
        break;
    }

    // Zero out the whitespace count if we've entered a scope.
    if (scope.length > 0)
      whitespace = 0;

    // Increment the index by the size of the character. Note that for Unicode
    // composite characters (like emoji) this will be more than one.
    index += character.length;
  }

  return selector.length - index + whitespace;
}

/**
 * Qualifies a CSS selector with a qualifier, which may be another CSS selector
 * or an empty string. For example, given the selector "div.bar" and the
 * qualifier "#foo", this function returns "div#foo.bar".
 * @param {string} selector The selector to qualify.
 * @param {string} qualifier The qualifier with which to qualify the selector.
 * @returns {string} The qualified selector.
 * @package
 */
exports.qualifySelector = function qualifySelector(selector, qualifier)
{
  let qualifiedSelector = "";

  let qualifierTargetSelectorIndex = findTargetSelectorIndex(qualifier);
  let [, qualifierType = ""] =
    /^([a-z][a-z-]*)?/i.exec(qualifier.substring(qualifierTargetSelectorIndex));

  for (let sub of splitSelector(selector))
  {
    sub = sub.trim();

    qualifiedSelector += ", ";

    let index = findTargetSelectorIndex(sub);

    // Note that the first group in the regular expression is optional. If it
    // doesn't match (e.g. "#foo::nth-child(1)"), type will be an empty string.
    let [, type = "", rest] =
      /^([a-z][a-z-]*)?\*?(.*)/i.exec(sub.substring(index));

    if (type == qualifierType)
      type = "";

    // If the qualifier ends in a combinator (e.g. "body #foo>"), we put the
    // type and the rest of the selector after the qualifier
    // (e.g. "body #foo>div.bar"); otherwise (e.g. "body #foo") we merge the
    // type into the qualifier (e.g. "body div#foo.bar").
    if (/[\s>+~]$/.test(qualifier))
      qualifiedSelector += sub.substring(0, index) + qualifier + type + rest;
    else
      qualifiedSelector += sub.substring(0, index) + type + qualifier + rest;
  }

  // Remove the initial comma and space.
  return qualifiedSelector.substring(2);
};


/***/ }),

/***/ 241:
/***/ ((__unused_webpack_module, exports, __webpack_require__) => {

var __webpack_unused_export__;
/*
 * This file is part of Adblock Plus <https://adblockplus.org/>,
 * Copyright (C) 2006-present eyeo GmbH
 *
 * Adblock Plus is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * Adblock Plus is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Adblock Plus.  If not, see <http://www.gnu.org/licenses/>.
 */

/** @module */



const {textToRegExp, filterToRegExp, splitSelector,
       qualifySelector} = __webpack_require__(173);

const MIN_INVOCATION_INTERVAL = 3000;
const MAX_SYNCHRONOUS_PROCESSING_TIME = 50;

let abpSelectorRegexp = /:-abp-([\w-]+)\(/i;

let testInfo = null;

function toCSSStyleDeclaration(value)
{
  return Object.assign(document.createElement("test"), {style: value}).style;
}

__webpack_unused_export__ = function setTestMode()
{
  testInfo = {
    lastProcessedElements: new Set()
  };
};

__webpack_unused_export__ = function getTestInfo()
{
  return testInfo;
};

function getCachedPropertyValue(object, name, defaultValueFunc = () => {})
{
  let value = object[name];
  if (typeof value == "undefined")
    Object.defineProperty(object, name, {value: value = defaultValueFunc()});
  return value;
}

/**
 * Return position of node from parent.
 * @param {Node} node the node to find the position of.
 * @return {number} One-based index like for :nth-child(), or 0 on error.
 */
function positionInParent(node)
{
  let index = 0;
  for (let child of node.parentNode.children)
  {
    if (child == node)
      return index + 1;

    index++;
  }

  return 0;
}

function makeSelector(node, selector = "")
{
  if (node == null)
    return null;
  if (!node.parentElement)
  {
    let newSelector = ":root";
    if (selector)
      newSelector += " > " + selector;
    return newSelector;
  }
  let idx = positionInParent(node);
  if (idx > 0)
  {
    let newSelector = `${node.tagName}:nth-child(${idx})`;
    if (selector)
      newSelector += " > " + selector;
    return makeSelector(node.parentElement, newSelector);
  }

  return selector;
}

function parseSelectorContent(content, startIndex)
{
  let parens = 1;
  let quote = null;
  let i = startIndex;
  for (; i < content.length; i++)
  {
    let c = content[i];
    if (c == "\\")
    {
      // Ignore escaped characters
      i++;
    }
    else if (quote)
    {
      if (c == quote)
        quote = null;
    }
    else if (c == "'" || c == '"')
    {
      quote = c;
    }
    else if (c == "(")
    {
      parens++;
    }
    else if (c == ")")
    {
      parens--;
      if (parens == 0)
        break;
    }
  }

  if (parens > 0)
    return null;
  return {text: content.substring(startIndex, i), end: i};
}

/**
 * Stringified style objects
 * @typedef {Object} StringifiedStyle
 * @property {string} style CSS style represented by a string.
 * @property {string[]} subSelectors selectors the CSS properties apply to.
 */

/**
 * Produce a string representation of the stylesheet entry.
 * @param {CSSStyleRule} rule the CSS style rule.
 * @return {StringifiedStyle} the stringified style.
 */
function stringifyStyle(rule)
{
  let styles = [];
  for (let i = 0; i < rule.style.length; i++)
  {
    let property = rule.style.item(i);
    let value = rule.style.getPropertyValue(property);
    let priority = rule.style.getPropertyPriority(property);
    styles.push(`${property}: ${value}${priority ? " !" + priority : ""};`);
  }
  styles.sort();
  return {
    style: styles.join(" "),
    subSelectors: splitSelector(rule.selectorText)
  };
}

let scopeSupported = null;

function tryQuerySelector(subtree, selector, all)
{
  let elements = null;
  try
  {
    elements = all ? subtree.querySelectorAll(selector) :
      subtree.querySelector(selector);
    scopeSupported = true;
  }
  catch (e)
  {
    // Edge doesn't support ":scope"
    scopeSupported = false;
  }
  return elements;
}

/**
 * Query selector.
 *
 * If it is relative, will try :scope.
 *
 * @param {Node} subtree the element to query selector
 * @param {string} selector the selector to query
 * @param {bool} [all=false] true to perform querySelectorAll()
 *
 * @returns {?(Node|NodeList)} result of the query. null in case of error.
 */
function scopedQuerySelector(subtree, selector, all)
{
  if (selector[0] == ">")
  {
    selector = ":scope" + selector;
    if (scopeSupported)
    {
      return all ? subtree.querySelectorAll(selector) :
        subtree.querySelector(selector);
    }
    if (scopeSupported == null)
      return tryQuerySelector(subtree, selector, all);
    return null;
  }
  return all ? subtree.querySelectorAll(selector) :
    subtree.querySelector(selector);
}

function scopedQuerySelectorAll(subtree, selector)
{
  return scopedQuerySelector(subtree, selector, true);
}

const regexpRegexp = /^\/(.*)\/([imu]*)$/;

/**
 * Make a regular expression from a text argument.
 *
 * If it can be parsed as a regular expression, parse it and the flags.
 *
 * @param {string} text the text argument.
 *
 * @return {?RegExp} a RegExp object or null in case of error.
 */
function makeRegExpParameter(text)
{
  let [, pattern, flags] =
      regexpRegexp.exec(text) || [null, textToRegExp(text)];

  try
  {
    return new RegExp(pattern, flags);
  }
  catch (e)
  {
  }
  return null;
}

function* evaluate(chain, index, prefix, subtree, styles, targets)
{
  if (index >= chain.length)
  {
    yield prefix;
    return;
  }
  for (let [selector, element] of chain[index].getSelectors(prefix, subtree,
                                                            styles, targets))
  {
    if (selector == null)
      yield null;
    else
      yield* evaluate(chain, index + 1, selector, element, styles, targets);
  }
  // Just in case the getSelectors() generator above had to run some heavy
  // document.querySelectorAll() call which didn't produce any results, make
  // sure there is at least one point where execution can pause.
  yield null;
}

class PlainSelector
{
  constructor(selector)
  {
    this._selector = selector;
    this.maybeDependsOnAttributes = /[#.]|\[.+\]/.test(selector);
    this.dependsOnDOM = this.maybeDependsOnAttributes;
    this.maybeContainsSiblingCombinators = /[~+]/.test(selector);
  }

  /**
   * Generator function returning a pair of selector string and subtree.
   * @param {string} prefix the prefix for the selector.
   * @param {Node} subtree the subtree we work on.
   * @param {StringifiedStyle[]} styles the stringified style objects.
   * @param {Node[]} [targets] the nodes we are interested in.
   */
  *getSelectors(prefix, subtree, styles, targets)
  {
    yield [prefix + this._selector, subtree];
  }
}

const incompletePrefixRegexp = /[\s>+~]$/;

class HasSelector
{
  constructor(selectors)
  {
    this.dependsOnDOM = true;

    this._innerSelectors = selectors;
  }

  get dependsOnStyles()
  {
    return this._innerSelectors.some(selector => selector.dependsOnStyles);
  }

  get dependsOnCharacterData()
  {
    return this._innerSelectors.some(
      selector => selector.dependsOnCharacterData
    );
  }

  get maybeDependsOnAttributes()
  {
    return this._innerSelectors.some(
      selector => selector.maybeDependsOnAttributes
    );
  }

  *getSelectors(prefix, subtree, styles, targets)
  {
    for (let element of this.getElements(prefix, subtree, styles, targets))
      yield [makeSelector(element), element];
  }

  /**
   * Generator function returning selected elements.
   * @param {string} prefix the prefix for the selector.
   * @param {Node} subtree the subtree we work on.
   * @param {StringifiedStyle[]} styles the stringified style objects.
   * @param {Node[]} [targets] the nodes we are interested in.
   */
  *getElements(prefix, subtree, styles, targets)
  {
    let actualPrefix = (!prefix || incompletePrefixRegexp.test(prefix)) ?
        prefix + "*" : prefix;
    let elements = scopedQuerySelectorAll(subtree, actualPrefix);
    if (elements)
    {
      for (let element of elements)
      {
        // If the element is neither an ancestor nor a descendant of one of the
        // targets, we can skip it.
        if (targets && !targets.some(target => element.contains(target) ||
                                               target.contains(element)))
        {
          yield null;
          continue;
        }

        let iter = evaluate(this._innerSelectors, 0, "", element, styles,
                            targets);
        for (let selector of iter)
        {
          if (selector == null)
            yield null;
          else if (scopedQuerySelector(element, selector))
            yield element;
        }
        yield null;

        if (testInfo)
          testInfo.lastProcessedElements.add(element);
      }
    }
  }
}

class ContainsSelector
{
  constructor(textContent)
  {
    this.dependsOnDOM = true;
    this.dependsOnCharacterData = true;

    this._regexp = makeRegExpParameter(textContent);
  }

  *getSelectors(prefix, subtree, styles, targets)
  {
    for (let element of this.getElements(prefix, subtree, styles, targets))
      yield [makeSelector(element), subtree];
  }

  *getElements(prefix, subtree, styles, targets)
  {
    let actualPrefix = (!prefix || incompletePrefixRegexp.test(prefix)) ?
        prefix + "*" : prefix;

    let elements = scopedQuerySelectorAll(subtree, actualPrefix);

    if (elements)
    {
      let lastRoot = null;
      for (let element of elements)
      {
        // For a filter like div:-abp-contains(Hello) and a subtree like
        // <div id="a"><div id="b"><div id="c">Hello</div></div></div>
        // we're only interested in div#a
        if (lastRoot && lastRoot.contains(element))
        {
          yield null;
          continue;
        }

        lastRoot = element;

        if (targets && !targets.some(target => element.contains(target) ||
                                               target.contains(element)))
        {
          yield null;
          continue;
        }

        if (this._regexp && this._regexp.test(element.textContent))
          yield element;
        else
          yield null;

        if (testInfo)
          testInfo.lastProcessedElements.add(element);
      }
    }
  }
}

class PropsSelector
{
  constructor(propertyExpression)
  {
    this.dependsOnStyles = true;
    this.dependsOnDOM = true;

    let regexpString;
    if (propertyExpression.length >= 2 && propertyExpression[0] == "/" &&
        propertyExpression[propertyExpression.length - 1] == "/")
      regexpString = propertyExpression.slice(1, -1);
    else
      regexpString = filterToRegExp(propertyExpression);

    this._regexp = new RegExp(regexpString, "i");
  }

  *findPropsSelectors(styles, prefix, regexp)
  {
    for (let style of styles)
    {
      if (regexp.test(style.style))
      {
        for (let subSelector of style.subSelectors)
        {
          if (subSelector.startsWith("*") &&
              !incompletePrefixRegexp.test(prefix))
            subSelector = subSelector.substring(1);

          let idx = subSelector.lastIndexOf("::");
          if (idx != -1)
            subSelector = subSelector.substring(0, idx);

          yield qualifySelector(subSelector, prefix);
        }
      }
    }
  }

  *getSelectors(prefix, subtree, styles, targets)
  {
    for (let selector of this.findPropsSelectors(styles, prefix, this._regexp))
      yield [selector, subtree];
  }
}

class Pattern
{
  constructor(selectors, text)
  {
    this.selectors = selectors;
    this.text = text;
  }

  get dependsOnStyles()
  {
    return getCachedPropertyValue(
      this, "_dependsOnStyles",
      () => this.selectors.some(selector => selector.dependsOnStyles)
    );
  }

  get dependsOnDOM()
  {
    return getCachedPropertyValue(
      this, "_dependsOnDOM",
      () => this.selectors.some(selector => selector.dependsOnDOM)
    );
  }

  get dependsOnStylesAndDOM()
  {
    return getCachedPropertyValue(
      this, "_dependsOnStylesAndDOM",
      () => this.selectors.some(selector => selector.dependsOnStyles &&
                                            selector.dependsOnDOM)
    );
  }

  get maybeDependsOnAttributes()
  {
    // Observe changes to attributes if either there's a plain selector that
    // looks like an ID selector, class selector, or attribute selector in one
    // of the patterns (e.g. "a[href='https://example.com/']")
    // or there's a properties selector nested inside a has selector
    // (e.g. "div:-abp-has(:-abp-properties(color: blue))")
    return getCachedPropertyValue(
      this, "_maybeDependsOnAttributes",
      () => this.selectors.some(
        selector => selector.maybeDependsOnAttributes ||
                    (selector instanceof HasSelector &&
                     selector.dependsOnStyles)
      )
    );
  }

  get dependsOnCharacterData()
  {
    // Observe changes to character data only if there's a contains selector in
    // one of the patterns.
    return getCachedPropertyValue(
      this, "_dependsOnCharacterData",
      () => this.selectors.some(selector => selector.dependsOnCharacterData)
    );
  }

  get maybeContainsSiblingCombinators()
  {
    return getCachedPropertyValue(
      this, "_maybeContainsSiblingCombinators",
      () => this.selectors.some(
        selector => selector.maybeContainsSiblingCombinators
      )
    );
  }

  matchesMutationTypes(mutationTypes)
  {
    let mutationTypeMatchMap = getCachedPropertyValue(
      this, "_mutationTypeMatchMap",
      () => new Map([
        // All types of DOM-dependent patterns are affected by mutations of
        // type "childList".
        ["childList", true],
        ["attributes", this.maybeDependsOnAttributes],
        ["characterData", this.dependsOnCharacterData]
      ])
    );

    for (let mutationType of mutationTypes)
    {
      if (mutationTypeMatchMap.get(mutationType))
        return true;
    }

    return false;
  }
}

function extractMutationTypes(mutations)
{
  let types = new Set();

  for (let mutation of mutations)
  {
    types.add(mutation.type);

    // There are only 3 types of mutations: "attributes", "characterData", and
    // "childList".
    if (types.size == 3)
      break;
  }

  return types;
}

function extractMutationTargets(mutations)
{
  if (!mutations)
    return null;

  let targets = new Set();

  for (let mutation of mutations)
  {
    if (mutation.type == "childList")
    {
      // When new nodes are added, we're interested in the added nodes rather
      // than the parent.
      for (let node of mutation.addedNodes)
        targets.add(node);
    }
    else
    {
      targets.add(mutation.target);
    }
  }

  return [...targets];
}

function filterPatterns(patterns, {stylesheets, mutations})
{
  if (!stylesheets && !mutations)
    return patterns.slice();

  let mutationTypes = mutations ? extractMutationTypes(mutations) : null;

  return patterns.filter(
    pattern => (stylesheets && pattern.dependsOnStyles) ||
               (mutations && pattern.dependsOnDOM &&
                pattern.matchesMutationTypes(mutationTypes))
  );
}

function shouldObserveAttributes(patterns)
{
  return patterns.some(pattern => pattern.maybeDependsOnAttributes);
}

function shouldObserveCharacterData(patterns)
{
  return patterns.some(pattern => pattern.dependsOnCharacterData);
}

exports.J$ = class ElemHideEmulation
{
  constructor(hideElemsFunc)
  {
    this._minInvocationInterval = MIN_INVOCATION_INTERVAL;
    this._filteringInProgress = false;
    this._lastInvocation = -MIN_INVOCATION_INTERVAL;
    this._scheduledProcessing = null;

    this.document = document;
    this.hideElemsFunc = hideElemsFunc;
    this.observer = new MutationObserver(this.observe.bind(this));
  }

  isSameOrigin(stylesheet)
  {
    try
    {
      return new URL(stylesheet.href).origin == this.document.location.origin;
    }
    catch (e)
    {
      // Invalid URL, assume that it is first-party.
      return true;
    }
  }

  /**
   * Parse the selector
   * @param {string} selector the selector to parse
   * @return {Array} selectors is an array of objects,
   * or null in case of errors.
   */
  parseSelector(selector)
  {
    if (selector.length == 0)
      return [];

    let match = abpSelectorRegexp.exec(selector);
    if (!match)
      return [new PlainSelector(selector)];

    let selectors = [];
    if (match.index > 0)
      selectors.push(new PlainSelector(selector.substring(0, match.index)));

    let startIndex = match.index + match[0].length;
    let content = parseSelectorContent(selector, startIndex);
    if (!content)
    {
      console.warn(new SyntaxError("Failed to parse Adblock Plus " +
                                   `selector ${selector} ` +
                                   "due to unmatched parentheses."));
      return null;
    }
    if (match[1] == "properties")
    {
      selectors.push(new PropsSelector(content.text));
    }
    else if (match[1] == "has")
    {
      let hasSelectors = this.parseSelector(content.text);
      if (hasSelectors == null)
        return null;
      selectors.push(new HasSelector(hasSelectors));
    }
    else if (match[1] == "contains")
    {
      selectors.push(new ContainsSelector(content.text));
    }
    else
    {
      // this is an error, can't parse selector.
      console.warn(new SyntaxError("Failed to parse Adblock Plus " +
                                   `selector ${selector}, invalid ` +
                                   `pseudo-class :-abp-${match[1]}().`));
      return null;
    }

    let suffix = this.parseSelector(selector.substring(content.end + 1));
    if (suffix == null)
      return null;

    selectors.push(...suffix);

    if (selectors.length == 1 && selectors[0] instanceof ContainsSelector)
    {
      console.warn(new SyntaxError("Failed to parse Adblock Plus " +
                                   `selector ${selector}, can't ` +
                                   "have a lonely :-abp-contains()."));
      return null;
    }
    return selectors;
  }

  /**
   * Processes the current document and applies all rules to it.
   * @param {CSSStyleSheet[]} [stylesheets]
   *    The list of new stylesheets that have been added to the document and
   *    made reprocessing necessary. This parameter shouldn't be passed in for
   *    the initial processing, all of document's stylesheets will be considered
   *    then and all rules, including the ones not dependent on styles.
   * @param {MutationRecord[]} [mutations]
   *    The list of DOM mutations that have been applied to the document and
   *    made reprocessing necessary. This parameter shouldn't be passed in for
   *    the initial processing, the entire document will be considered
   *    then and all rules, including the ones not dependent on the DOM.
   * @param {function} [done]
   *    Callback to call when done.
   */
  _addSelectors(stylesheets, mutations, done)
  {
    if (testInfo)
      testInfo.lastProcessedElements.clear();

    let patterns = filterPatterns(this.patterns, {stylesheets, mutations});

    let elements = [];
    let elementFilters = [];

    let cssStyles = [];

    // If neither any style sheets nor any DOM mutations have been specified,
    // do full processing.
    if (!stylesheets && !mutations)
      stylesheets = this.document.styleSheets;

    // If there are any DOM mutations and any of the patterns depends on both
    // style sheets and the DOM (e.g. -abp-has(-abp-properties)), find all the
    // rules in every style sheet in the document, because we need to run
    // querySelectorAll afterwards. On the other hand, if we only have patterns
    // that depend on either styles or DOM both not both (e.g. -abp-contains),
    // we can skip this part.
    if (mutations && patterns.some(pattern => pattern.dependsOnStylesAndDOM))
      stylesheets = this.document.styleSheets;

    for (let stylesheet of stylesheets || [])
    {
      // Explicitly ignore third-party stylesheets to ensure consistent behavior
      // between Firefox and Chrome.
      if (!this.isSameOrigin(stylesheet))
        continue;

      let rules;
      try
      {
        rules = stylesheet.cssRules;
      }
      catch (e)
      {
        // On Firefox, there is a chance that an InvalidAccessError
        // get thrown when accessing cssRules. Just skip the stylesheet
        // in that case.
        // See https://searchfox.org/mozilla-central/rev/f65d7528e34ef1a7665b4a1a7b7cdb1388fcd3aa/layout/style/StyleSheet.cpp#699
        continue;
      }

      if (!rules)
        continue;

      for (let rule of rules)
      {
        if (rule.type != rule.STYLE_RULE)
          continue;

        cssStyles.push(stringifyStyle(rule));
      }
    }

    let targets = extractMutationTargets(mutations);

    let pattern = null;
    let generator = null;

    let processPatterns = () =>
    {
      let cycleStart = performance.now();

      if (!pattern)
      {
        if (!patterns.length)
        {
          if (elements.length > 0)
            this.hideElemsFunc(elements, elementFilters);
          if (typeof done == "function")
            done();
          return;
        }

        pattern = patterns.shift();

        let evaluationTargets = targets;

        // If the pattern appears to contain any sibling combinators, we can't
        // easily optimize based on the mutation targets. Since this is a
        // special case, skip the optimization. By setting it to null here we
        // make sure we process the entire DOM.
        if (pattern.maybeContainsSiblingCombinators)
          evaluationTargets = null;

        generator = evaluate(pattern.selectors, 0, "",
                             this.document, cssStyles, evaluationTargets);
      }
      for (let selector of generator)
      {
        if (selector != null)
        {
          for (let element of this.document.querySelectorAll(selector))
          {
            elements.push(element);
            elementFilters.push(pattern.text);
          }
        }
        if (performance.now() - cycleStart > MAX_SYNCHRONOUS_PROCESSING_TIME)
        {
          setTimeout(processPatterns, 0);
          return;
        }
      }
      pattern = null;
      return processPatterns();
    };

    processPatterns();
  }

  // This property is only used in the tests
  // to shorten the invocation interval
  get minInvocationInterval()
  {
    return this._minInvocationInterval;
  }

  set minInvocationInterval(interval)
  {
    this._minInvocationInterval = interval;
  }

  /**
   * Re-run filtering either immediately or queued.
   * @param {CSSStyleSheet[]} [stylesheets]
   *    new stylesheets to be processed. This parameter should be omitted
   *    for full reprocessing.
   * @param {MutationRecord[]} [mutations]
   *    new DOM mutations to be processed. This parameter should be omitted
   *    for full reprocessing.
   */
  queueFiltering(stylesheets, mutations)
  {
    let completion = () =>
    {
      this._lastInvocation = performance.now();
      this._filteringInProgress = false;
      if (this._scheduledProcessing)
      {
        let params = Object.assign({}, this._scheduledProcessing);
        this._scheduledProcessing = null;
        this.queueFiltering(params.stylesheets, params.mutations);
      }
    };

    if (this._scheduledProcessing)
    {
      if (!stylesheets && !mutations)
      {
        this._scheduledProcessing = {};
      }
      else if (this._scheduledProcessing.stylesheets ||
               this._scheduledProcessing.mutations)
      {
        if (stylesheets)
        {
          if (!this._scheduledProcessing.stylesheets)
            this._scheduledProcessing.stylesheets = [];
          this._scheduledProcessing.stylesheets.push(...stylesheets);
        }
        if (mutations)
        {
          if (!this._scheduledProcessing.mutations)
            this._scheduledProcessing.mutations = [];
          this._scheduledProcessing.mutations.push(...mutations);
        }
      }
    }
    else if (this._filteringInProgress)
    {
      this._scheduledProcessing = {stylesheets, mutations};
    }
    else if (performance.now() - this._lastInvocation <
             this.minInvocationInterval)
    {
      this._scheduledProcessing = {stylesheets, mutations};
      setTimeout(
        () =>
        {
          let params = Object.assign({}, this._scheduledProcessing);
          this._filteringInProgress = true;
          this._scheduledProcessing = null;
          this._addSelectors(params.stylesheets, params.mutations, completion);
        },
        this.minInvocationInterval - (performance.now() - this._lastInvocation)
      );
    }
    else if (this.document.readyState == "loading")
    {
      this._scheduledProcessing = {stylesheets, mutations};
      let handler = () =>
      {
        this.document.removeEventListener("DOMContentLoaded", handler);
        let params = Object.assign({}, this._scheduledProcessing);
        this._filteringInProgress = true;
        this._scheduledProcessing = null;
        this._addSelectors(params.stylesheets, params.mutations, completion);
      };
      this.document.addEventListener("DOMContentLoaded", handler);
    }
    else
    {
      this._filteringInProgress = true;
      this._addSelectors(stylesheets, mutations, completion);
    }
  }

  onLoad(event)
  {
    let stylesheet = event.target.sheet;
    if (stylesheet)
      this.queueFiltering([stylesheet]);
  }

  observe(mutations)
  {
    if (testInfo)
    {
      // In test mode, filter out any mutations likely done by us
      // (i.e. style="display: none !important"). This makes it easier to
      // observe how the code responds to DOM mutations.
      mutations = mutations.filter(
        ({type, attributeName, target: {style: newValue}, oldValue}) =>
          !(type == "attributes" && attributeName == "style" &&
            newValue.display == "none" &&
            toCSSStyleDeclaration(oldValue).display != "none")
      );

      if (mutations.length == 0)
        return;
    }

    this.queueFiltering(null, mutations);
  }

  apply(patterns)
  {
    this.patterns = [];
    for (let pattern of patterns)
    {
      let selectors = this.parseSelector(pattern.selector);
      if (selectors != null && selectors.length > 0)
        this.patterns.push(new Pattern(selectors, pattern.text));
    }

    if (this.patterns.length > 0)
    {
      this.queueFiltering();
      let attributes = shouldObserveAttributes(this.patterns);
      this.observer.observe(
        this.document,
        {
          childList: true,
          attributes,
          attributeOldValue: attributes && !!testInfo,
          characterData: shouldObserveCharacterData(this.patterns),
          subtree: true
        }
      );
      this.document.addEventListener("load", this.onLoad.bind(this), true);
    }
  }
};


/***/ }),

/***/ 659:
/***/ ((__unused_webpack___webpack_module__, __unused_webpack___webpack_exports__, __webpack_require__) => {

/* harmony import */ var _include_preload_js__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(334);
/*
 * This file is part of Adblock Plus <https://adblockplus.org/>,
 * Copyright (C) 2006-present eyeo GmbH
 *
 * Adblock Plus is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * Adblock Plus is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Adblock Plus.  If not, see <http://www.gnu.org/licenses/>.
 */

;

// The page ID for the popup filter selection dialog (top frame only).
let blockelementPopupId = null;

// Element picking state (top frame only).
let currentlyPickingElement = false;
let lastMouseOverEvent = null;

// During element picking this is the currently highlighted element. When
// element has been picked this is the element that is due to be blocked.
let currentElement = null;

// Highlighting state, used by the top frame during element picking and all
// frames when the chosen element is highlighted red.
let highlightedElementsSelector = null;
let highlightedElementsInterval = null;

// Last right click state stored for element blocking via the context menu.
let lastRightClickEvent = null;
let lastRightClickEventIsMostRecent = false;


/* Utilities */

function getFiltersForElement(element)
{
  let src = element.getAttribute("src");
  return browser.runtime.sendMessage({
    type: "composer.getFilters",
    tagName: element.localName,
    id: element.id,
    src: src && src.length <= 1000 ? src : null,
    style: element.getAttribute("style"),
    classes: Array.prototype.slice.call(element.classList),
    url: (0,_include_preload_js__WEBPACK_IMPORTED_MODULE_0__/* .getURLFromElement */ .Um)(element)
  });
}

async function getBlockableElementOrAncestor(element)
{
  // We assume that the user doesn't want to block the whole page.
  // So we never consider the <html> or <body> element.
  while (element && element != document.documentElement &&
         element != document.body)
  {
    // We can't handle non-HTML (like SVG) elements, as well as
    // <area> elements (see below). So fall back to the parent element.
    if (!(element instanceof HTMLElement) || element.localName == "area")
    {
      element = element.parentElement;
    }
    // If image maps are used mouse events occur for the <area> element.
    // But we have to block the image associated with the <map> element.
    else if (element.localName == "map")
    {
      let images = document.querySelectorAll("img[usemap]");
      let image = null;

      for (let currentImage of images)
      {
        let usemap = currentImage.getAttribute("usemap");
        let index = usemap.indexOf("#");

        if (index != -1 && usemap.substr(index + 1) == element.name)
        {
          image = currentImage;
          break;
        }
      }

      element = image;
    }

    // Finally, if none of the above is true, check whether we can generate
    // any filters for this element. Otherwise fall back to its parent element.
    else
    {
      let {filters} = await getFiltersForElement(element);
      if (filters.length > 0)
        return element;
      return getBlockableElementOrAncestor(element.parentElement);
    }
  }

  // We reached the document root without finding a blockable element.
  return null;
}


/* Element highlighting */

// Adds an overlay to an element in order to highlight it.
function addElementOverlay(element)
{
  let position = "absolute";
  let offsetX = window.scrollX;
  let offsetY = window.scrollY;

  for (let e = element; e; e = e.parentElement)
  {
    let style = getComputedStyle(e);

    // If the element isn't rendered (since its or one of its ancestor's
    // "display" property is "none"), the overlay wouldn't match the element.
    if (style.display == "none")
      return null;

    // If the element or one of its ancestors uses fixed postioning, the overlay
    // must too. Otherwise its position might not match the element's.
    if (style.position == "fixed")
    {
      position = "fixed";
      offsetX = offsetY = 0;
    }
  }

  let overlay = document.createElement("div");
  overlay.prisoner = element;
  overlay.className = "__adblockplus__overlay";
  overlay.setAttribute("style",
                       "opacity:0.4; display:inline-block !important; " +
                       "overflow:hidden; box-sizing:border-box;");
  let rect = element.getBoundingClientRect();
  overlay.style.width = rect.width + "px";
  overlay.style.height = rect.height + "px";
  overlay.style.left = (rect.left + offsetX) + "px";
  overlay.style.top = (rect.top + offsetY) + "px";
  overlay.style.position = position;
  overlay.style.zIndex = 0x7FFFFFFE;

  document.documentElement.appendChild(overlay);
  return overlay;
}

function highlightElement(element, border, backgroundColor)
{
  unhighlightElement(element);

  let highlightWithOverlay = () =>
  {
    let overlay = addElementOverlay(element);

    // If the element isn't displayed no overlay will be added.
    // Moreover, we don't need to highlight anything then.
    if (!overlay)
      return;

    highlightElement(overlay, border, backgroundColor);
    overlay.style.pointerEvents = "none";

    element._unhighlight = () =>
    {
      overlay.parentNode.removeChild(overlay);
    };
  };

  let highlightWithStyleAttribute = () =>
  {
    let originalBorder = element.style.getPropertyValue("border");
    let originalBorderPriority =
      element.style.getPropertyPriority("box-shadow");
    let originalBackgroundColor =
      element.style.getPropertyValue("background-color");
    let originalBackgroundColorPriority =
      element.style.getPropertyPriority("background-color");

    element.style.setProperty("border", `2px solid ${border}`, "important");
    element.style.setProperty("background-color", backgroundColor, "important");

    element._unhighlight = () =>
    {
      element.style.removeProperty("box-shadow");
      element.style.setProperty(
        "border",
        originalBorder,
        originalBorderPriority
      );

      element.style.removeProperty("background-color");
      element.style.setProperty(
        "background-color",
        originalBackgroundColor,
        originalBackgroundColorPriority
      );
    };
  };

  // If this element is an overlay that we've created previously then we need
  // to give it a background colour. Otherwise we need to create an overlay
  // and then recurse in order to set the overlay's background colour.
  if ("prisoner" in element)
    highlightWithStyleAttribute();
  else
    highlightWithOverlay();
}

function unhighlightElement(element)
{
  if (element && "_unhighlight" in element)
  {
    element._unhighlight();
    delete element._unhighlight;
  }
}

// Highlight elements matching the selector string red.
// (All elements that would be blocked by the proposed filters.)
function highlightElements(selectorString)
{
  unhighlightElements();

  let elements = Array.prototype.slice.call(
    document.querySelectorAll(selectorString)
  );
  highlightedElementsSelector = selectorString;

  // Highlight elements progressively. Otherwise the page freezes
  // when a lot of elements get highlighted at the same time.
  highlightedElementsInterval = setInterval(() =>
  {
    if (elements.length > 0)
    {
      let element = elements.shift();
      if (element != currentElement)
        highlightElement(element, "#CA0000", "#CA0000");
    }
    else
    {
      clearInterval(highlightedElementsInterval);
      highlightedElementsInterval = null;
    }
  }, 0);
}

// Unhighlight the elements that were highlighted by selector string previously.
function unhighlightElements()
{
  if (highlightedElementsInterval)
  {
    clearInterval(highlightedElementsInterval);
    highlightedElementsInterval = null;
  }

  if (highlightedElementsSelector)
  {
    Array.prototype.forEach.call(
      document.querySelectorAll(highlightedElementsSelector),
      unhighlightElement
    );

    highlightedElementsSelector = null;
  }
}


/* Input event handlers */

function stopEventPropagation(event)
{
  event.stopPropagation();
}

// Hovering over an element so highlight it.
async function mouseOver(event)
{
  lastMouseOverEvent = event;

  let element = await getBlockableElementOrAncestor(event.target);
  if (event == lastMouseOverEvent)
  {
    lastMouseOverEvent = null;

    if (currentlyPickingElement)
    {
      if (currentElement)
        unhighlightElement(currentElement);

      if (element)
        highlightElement(element, "#CA0000", "#CA0000");

      currentElement = element;
    }
  }

  event.stopPropagation();
}

// No longer hovering over this element so unhighlight it.
function mouseOut(event)
{
  if (!currentlyPickingElement || currentElement != event.target)
    return;

  unhighlightElement(currentElement);
  event.stopPropagation();
}

// Key events - Return selects currently hovered-over element, escape aborts.
function keyDown(event)
{
  if (!event.ctrlKey && !event.altKey && !event.shiftKey)
  {
    if (event.keyCode == 13) // Return
      elementPicked(event);
    else if (event.keyCode == 27) // Escape
      deactivateBlockElement();
  }
}


/* Element selection */

// Start highlighting elements yellow as the mouse moves over them, when one is
// chosen launch the popup dialog for the user to confirm the generated filters.
function startPickingElement()
{
  currentlyPickingElement = true;

  // Add (currently invisible) overlays for blockable elements that don't emit
  // mouse events, so that they can still be selected.
  Array.prototype.forEach.call(
    document.querySelectorAll("object,embed,iframe,frame"),
    async element =>
    {
      let {filters} = await getFiltersForElement(element);
      if (filters.length > 0)
        addElementOverlay(element);
    }
  );

  document.addEventListener("mousedown", stopEventPropagation, true);
  document.addEventListener("mouseup", stopEventPropagation, true);
  document.addEventListener("mouseenter", stopEventPropagation, true);
  document.addEventListener("mouseleave", stopEventPropagation, true);
  document.addEventListener("mouseover", mouseOver, true);
  document.addEventListener("mouseout", mouseOut, true);
  document.addEventListener("click", elementPicked, true);
  document.addEventListener("contextmenu", elementPicked, true);
  document.addEventListener("keydown", keyDown, true);

  ext.onExtensionUnloaded.addListener(deactivateBlockElement);
}

// Used to hide/show blocked elements on composer.content.preview
async function previewBlockedElements(active)
{
  if (!currentElement)
    return;

  let element = currentElement.prisoner || currentElement;
  let overlays = document.querySelectorAll(".__adblockplus__overlay");

  previewBlockedElement(element, active, overlays);

  let {selectors} = await getFiltersForElement(element);
  if (selectors.length > 0)
  {
    let cssQuery = selectors.join(",");
    for (let node of document.querySelectorAll(cssQuery))
      previewBlockedElement(node, active, overlays);
  }
}

// the previewBlockedElements helper to avoid duplicated code
function previewBlockedElement(element, active, overlays)
{
  let display = active ? "none" : null;
  let find = Array.prototype.find;
  let overlay = find.call(overlays, ({prisoner}) => prisoner === element);
  if (overlay)
    overlay.style.display = display;
  element.style.display = display;
}

// The user has picked an element - currentElement. Highlight it red, generate
// filters for it and open a popup dialog so that the user can confirm.
async function elementPicked(event)
{
  if (!currentElement)
    return;

  event.preventDefault();
  event.stopPropagation();

  let element = currentElement.prisoner || currentElement;
  let {filters, selectors} = await getFiltersForElement(element);
  if (currentlyPickingElement)
    stopPickingElement();

  highlightElement(currentElement, "#CA0000", "#CA0000");

  let highlights = 1;
  if (selectors.length > 0)
  {
    let cssQuery = selectors.join(",");
    highlightElements(cssQuery);
    highlights = document.querySelectorAll(cssQuery).length;
  }

  let popupId = await browser.runtime.sendMessage({
    type: "composer.openDialog",
    filters,
    highlights
  });
  // Only the top frame keeps a record of the popup window's ID,
  // so if this isn't the top frame we need to pass the ID on.
  if (window == window.top)
  {
    blockelementPopupId = popupId;
  }
  else
  {
    browser.runtime.sendMessage({
      type: "composer.forward",
      payload: {type: "composer.content.dialogOpened", popupId}
    });
  }
}

function stopPickingElement()
{
  currentlyPickingElement = false;

  document.removeEventListener("mousedown", stopEventPropagation, true);
  document.removeEventListener("mouseup", stopEventPropagation, true);
  document.removeEventListener("mouseenter", stopEventPropagation, true);
  document.removeEventListener("mouseleave", stopEventPropagation, true);
  document.removeEventListener("mouseover", mouseOver, true);
  document.removeEventListener("mouseout", mouseOut, true);
  document.removeEventListener("click", elementPicked, true);
  document.removeEventListener("contextmenu", elementPicked, true);
  document.removeEventListener("keydown", keyDown, true);
}


/* Core logic */

// We're done with the block element feature for now, tidy everything up.
function deactivateBlockElement(popupAlreadyClosed)
{
  previewBlockedElements(false);

  if (currentlyPickingElement)
    stopPickingElement();

  if (blockelementPopupId != null && !popupAlreadyClosed)
  {
    browser.runtime.sendMessage({
      type: "composer.forward",
      targetPageId: blockelementPopupId,
      payload:
      {
        type: "composer.dialog.close"
      }
    });
  }

  blockelementPopupId = null;
  lastRightClickEvent = null;

  if (currentElement)
  {
    unhighlightElement(currentElement);
    currentElement = null;
  }
  unhighlightElements();

  let overlays = document.getElementsByClassName("__adblockplus__overlay");
  while (overlays.length > 0)
    overlays[0].parentNode.removeChild(overlays[0]);

  ext.onExtensionUnloaded.removeListener(deactivateBlockElement);
}

function initializeComposer()
{
  // Use a contextmenu handler to save the last element the user right-clicked
  // on. To make things easier, we actually save the DOM event. We have to do
  // this because the contextMenu API only provides a URL, not the actual DOM
  // element.
  //   We also need to make sure that the previous right click event,
  // if there is one, is removed. We don't know which frame it is in so we must
  // send a message to the other frames to clear their old right click events.
  document.addEventListener("contextmenu", event =>
  {
    lastRightClickEvent = event;
    lastRightClickEventIsMostRecent = true;

    browser.runtime.sendMessage({
      type: "composer.forward",
      payload:
      {
        type: "composer.content.clearPreviousRightClickEvent"
      }
    });
  }, true);

  ext.onMessage.addListener(async(message, sender, sendResponse) =>
  {
    switch (message.type)
    {
      case "composer.content.preview":
        previewBlockedElements(message.active);
        break;
      case "composer.content.getState":
        if (window == window.top)
        {
          sendResponse({
            active: currentlyPickingElement || blockelementPopupId != null
          });
        }
        break;
      case "composer.content.startPickingElement":
        if (window == window.top)
          startPickingElement();
        break;
      case "composer.content.contextMenuClicked":
        let event = lastRightClickEvent;
        deactivateBlockElement();
        if (event)
        {
          getBlockableElementOrAncestor(event.target).then(element =>
          {
            if (element)
            {
              currentElement = element;
              elementPicked(event);
            }
          });
        }
        break;
      case "composer.content.finished":
        if (currentElement && message.remove)
        {
          // Hide the selected element itself. Note that this
          // behavior is incomplete, but the best we can do here,
          // e.g. if an added blocking filter matches other elements,
          // the effect won't be visible until the page is is reloaded.
          (0,_include_preload_js__WEBPACK_IMPORTED_MODULE_0__/* .collapseElement */ .Ot)(currentElement.prisoner || currentElement);

          // Apply added element hiding filters.
          _include_preload_js__WEBPACK_IMPORTED_MODULE_0__/* .contentFiltering.apply */ .YR.apply({elemhide: true});
        }
        deactivateBlockElement(!!message.popupAlreadyClosed);
        break;
      case "composer.content.clearPreviousRightClickEvent":
        if (!lastRightClickEventIsMostRecent)
          lastRightClickEvent = null;
        lastRightClickEventIsMostRecent = false;
        break;
      case "composer.content.dialogOpened":
        if (window == window.top)
          blockelementPopupId = message.popupId;
        break;
      case "composer.content.dialogClosed":
        // The onRemoved hook for the popup can create a race condition, so we
        // to be careful here. (This is not perfect, but best we can do.)
        if (window == window.top && blockelementPopupId == message.popupId)
        {
          browser.runtime.sendMessage({
            type: "composer.forward",
            payload:
            {
              type: "composer.content.finished",
              popupAlreadyClosed: true
            }
          });
        }
        break;
    }
  });

  if (window == window.top)
    browser.runtime.sendMessage({type: "composer.ready"});
}

if (document instanceof HTMLDocument)
  initializeComposer();


/***/ }),

/***/ 334:
/***/ ((__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) => {

/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "YR": () => /* binding */ contentFiltering,
/* harmony export */   "Um": () => /* binding */ getURLFromElement,
/* harmony export */   "Ot": () => /* binding */ collapseElement
/* harmony export */ });
/* harmony import */ var _adblockpluscore_lib_content_elemHideEmulation_js__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(241);
/*
 * This file is part of Adblock Plus <https://adblockplus.org/>,
 * Copyright (C) 2006-present eyeo GmbH
 *
 * Adblock Plus is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * Adblock Plus is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Adblock Plus.  If not, see <http://www.gnu.org/licenses/>.
 */

;

let contentFiltering;
let collapsedSelectors = new Set();

function getURLFromElement(element)
{
  if (element.localName == "object")
  {
    if (element.data)
      return element.data;

    for (let child of element.children)
    {
      if (child.localName == "param" && child.name == "movie" && child.value)
        return new URL(child.value, document.baseURI).href;
    }

    return null;
  }

  return element.currentSrc || element.src;
}

function getSelectorForBlockedElement(element)
{
  // Setting the "display" CSS property to "none" doesn't have any effect on
  // <frame> elements (in framesets). So we have to hide it inline through
  // the "visibility" CSS property.
  if (element.localName == "frame")
    return null;

  // If the <video> or <audio> element contains any <source> children,
  // we cannot address it in CSS by the source URL; in that case we
  // don't "collapse" it using a CSS selector but rather hide it directly by
  // setting the style="..." attribute.
  if (element.localName == "video" || element.localName == "audio")
  {
    for (let child of element.children)
    {
      if (child.localName == "source")
        return null;
    }
  }

  let selector = "";
  for (let attr of ["src", "srcset"])
  {
    let value = element.getAttribute(attr);
    if (value && attr in element)
      selector += "[" + attr + "=" + CSS.escape(value) + "]";
  }

  return selector ? element.localName + selector : null;
}

function hideElement(element, properties)
{
  let {style} = element;
  let actualProperties = [];

  if (element.localName == "frame")
    actualProperties = properties = [["visibility", "hidden"]];
  else if (!properties)
    actualProperties = properties = [["display", "none"]];

  for (let [key, value] of properties)
    style.setProperty(key, value, "important");

  if (!actualProperties)
  {
    actualProperties = [];
    for (let [key] of properties)
      actualProperties.push([key, style.getPropertyValue(key)]);
  }

  new MutationObserver(() =>
  {
    for (let [key, value] of actualProperties)
    {
      if (style.getPropertyValue(key) != value ||
          style.getPropertyPriority(key) != "important")
        style.setProperty(key, value, "important");
    }
  }).observe(
    element, {
      attributes: true,
      attributeFilter: ["style"]
    }
  );
}

function collapseElement(element)
{
  let selector = getSelectorForBlockedElement(element);
  if (selector)
  {
    if (!collapsedSelectors.has(selector))
    {
      contentFiltering.addSelectors([selector], "collapsing", true);
      collapsedSelectors.add(selector);
    }
  }
  else
  {
    hideElement(element);
  }
}

function startElementCollapsing()
{
  let deferred = null;

  browser.runtime.onMessage.addListener((message, sender) =>
  {
    if (message.type != "filters.collapse")
      return;

    if (document.readyState == "loading")
    {
      if (!deferred)
      {
        deferred = new Map();
        document.addEventListener("DOMContentLoaded", () =>
        {
          for (let [selector, urls] of deferred)
          {
            for (let element of document.querySelectorAll(selector))
            {
              if (urls.has(getURLFromElement(element)))
                collapseElement(element);
            }
          }

          deferred = null;
        });
      }

      let urls = deferred.get(message.selector) || new Set();
      deferred.set(message.selector, urls);
      urls.add(message.url);
    }
    else
    {
      for (let element of document.querySelectorAll(message.selector))
      {
        if (getURLFromElement(element) == message.url)
          collapseElement(element);
      }
    }
  });
}

function checkSitekey()
{
  let attr = document.documentElement.getAttribute("data-adblockkey");
  if (attr)
    browser.runtime.sendMessage({type: "filters.addKey", token: attr});
}

class ElementHidingTracer
{
  constructor(selectors, exceptions)
  {
    this.selectors = selectors;
    this.exceptions = exceptions;
    this.changedNodes = [];
    this.timeout = null;
    this.observer = new MutationObserver(this.observe.bind(this));
    this.trace = this.trace.bind(this);

    if (document.readyState == "loading")
      document.addEventListener("DOMContentLoaded", this.trace);
    else
      this.trace();
  }

  checkNodes(nodes)
  {
    let effectiveSelectors = [];
    let effectiveExceptions = [];

    for (let selector of this.selectors)
    {
      for (let node of nodes)
      {
        if (node.querySelector(selector))
        {
          effectiveSelectors.push(selector);
          break;
        }
      }
    }

    for (let exception of this.exceptions)
    {
      for (let node of nodes)
      {
        if (node.querySelector(exception.selector))
        {
          effectiveExceptions.push(exception.text);
          break;
        }
      }
    }

    if (effectiveSelectors.length > 0 || effectiveExceptions.length > 0)
    {
      browser.runtime.sendMessage({
        type: "hitLogger.traceElemHide",
        selectors: effectiveSelectors,
        filters: effectiveExceptions
      });
    }
  }

  onTimeout()
  {
    this.checkNodes(this.changedNodes);
    this.changedNodes = [];
    this.timeout = null;
  }

  observe(mutations)
  {
    // Forget previously changed nodes that are no longer in the DOM.
    for (let i = 0; i < this.changedNodes.length; i++)
    {
      if (!document.contains(this.changedNodes[i]))
        this.changedNodes.splice(i--, 1);
    }

    for (let mutation of mutations)
    {
      let node = mutation.target;

      // Ignore mutations of nodes that aren't in the DOM anymore.
      if (!document.contains(node))
        continue;

      // Since querySelectorAll() doesn't consider the root itself
      // and since CSS selectors can also match siblings, we have
      // to consider the parent node for attribute mutations.
      if (mutation.type == "attributes")
        node = node.parentNode;

      let addNode = true;
      for (let i = 0; i < this.changedNodes.length; i++)
      {
        let previouslyChangedNode = this.changedNodes[i];

        // If we are already going to check an ancestor of this node,
        // we can ignore this node, since it will be considered anyway
        // when checking one of its ancestors.
        if (previouslyChangedNode.contains(node))
        {
          addNode = false;
          break;
        }

        // If this node is an ancestor of a node that previously changed,
        // we can ignore that node, since it will be considered anyway
        // when checking one of its ancestors.
        if (node.contains(previouslyChangedNode))
          this.changedNodes.splice(i--, 1);
      }

      if (addNode)
        this.changedNodes.push(node);
    }

    // Check only nodes whose descendants have changed, and not more often
    // than once a second. Otherwise large pages with a lot of DOM mutations
    // (like YouTube) freeze when the devtools panel is active.
    if (this.timeout == null)
      this.timeout = setTimeout(this.onTimeout.bind(this), 1000);
  }

  trace()
  {
    this.checkNodes([document]);

    this.observer.observe(
      document,
      {
        childList: true,
        attributes: true,
        subtree: true
      }
    );
  }

  disconnect()
  {
    document.removeEventListener("DOMContentLoaded", this.trace);
    this.observer.disconnect();
    clearTimeout(this.timeout);
  }
}

class ContentFiltering
{
  constructor()
  {
    this.styles = new Map();
    this.tracer = null;
    this.cssProperties = null;
    this.elemHideEmulation =
      new _adblockpluscore_lib_content_elemHideEmulation_js__WEBPACK_IMPORTED_MODULE_0__/* .ElemHideEmulation */ .J$(this.hideElements.bind(this));
  }

  addRulesInline(rules, groupName = "standard", appendOnly = false)
  {
    let style = this.styles.get(groupName);

    if (style && !appendOnly)
    {
      while (style.sheet.cssRules.length > 0)
        style.sheet.deleteRule(0);
    }

    if (rules.length == 0)
      return;

    if (!style)
    {
      // Create <style> element lazily, only if we add styles. Add it to
      // the <head> or <html> element. If we have injected a style element
      // before that has been removed (the sheet property is null), create a
      // new one.
      style = document.createElement("style");
      (document.head || document.documentElement).appendChild(style);

      // It can happen that the frame already navigated to a different
      // document while we were waiting for the background page to respond.
      // In that case the sheet property may stay null, after adding the
      // <style> element.
      if (!style.sheet)
        return;

      this.styles.set(groupName, style);
    }

    for (let rule of rules)
      style.sheet.insertRule(rule, style.sheet.cssRules.length);
  }

  async addSelectors(selectors, groupName = "standard", appendOnly = false)
  {
    let rules = await browser.runtime.sendMessage({
      type: "content.injectSelectors",
      selectors,
      groupName,
      appendOnly
    });
    if (rules)
    {
      // Insert the rules inline if we have been instructed by the background
      // page to do so. This is rarely the case, except on platforms that do
      // not support user stylesheets via the browser.tabs.insertCSS API, i.e.
      // Firefox <53 and Chrome <66.
      // Once all supported platforms have implemented this API, we can remove
      // the code below. See issue #5090.
      // Related Chrome and Firefox issues:
      // https://bugs.chromium.org/p/chromium/issues/detail?id=632009
      // https://bugzilla.mozilla.org/show_bug.cgi?id=1310026
      this.addRulesInline(rules, groupName, appendOnly);
    }
  }

  hideElements(elements, filters)
  {
    for (let element of elements)
      hideElement(element, this.cssProperties);

    if (this.tracer)
    {
      browser.runtime.sendMessage({
        type: "hitLogger.traceElemHide",
        selectors: [],
        filters
      });
    }
  }

  async apply(filterTypes)
  {
    let response = await browser.runtime.sendMessage({
      type: "content.applyFilters",
      filterTypes
    });

    if (this.tracer)
    {
      this.tracer.disconnect();
      this.tracer = null;
    }

    if (response.inline)
      this.addRulesInline(response.rules);

    if (response.trace)
    {
      this.tracer = new ElementHidingTracer(
        response.selectors,
        response.exceptions
      );
    }

    this.cssProperties = response.cssProperties;
    this.elemHideEmulation.apply(response.emulatedPatterns);
  }
}

if (document instanceof HTMLDocument)
{
  checkSitekey();

  contentFiltering = new ContentFiltering();
  contentFiltering.apply();

  startElementCollapsing();
}


/***/ }),

/***/ 487:
/***/ (() => {

/*
 * This file is part of Adblock Plus <https://adblockplus.org/>,
 * Copyright (C) 2006-present eyeo GmbH
 *
 * Adblock Plus is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * Adblock Plus is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Adblock Plus.  If not, see <http://www.gnu.org/licenses/>.
 */

let randomEventName = "abp-request-" + Math.random().toString(36).substr(2);

// Proxy "should we block?" messages from checkRequest inside the injected
// code to the background page and back again.
document.addEventListener(randomEventName, async event =>
{
  let {url} = event.detail;

  let block = await browser.runtime.sendMessage({
    type: "request.blockedByRTCWrapper",
    url
  });
  document.dispatchEvent(new CustomEvent(
    randomEventName + "-" + url, {detail: block}
  ));
});

function injected(eventName, injectedIntoContentWindow)
{
  let checkRequest;

  /*
   * Frame context wrapper
   *
   * For some edge-cases Chrome will not run content scripts inside of frames.
   * Website have started to abuse this fact to access unwrapped APIs via a
   * frame's contentWindow (#4586, 5207). Therefore until Chrome runs content
   * scripts consistently for all frames we must take care to (re)inject our
   * wrappers when the contentWindow is accessed.
   */
  let injectedToString = Function.prototype.toString.bind(injected);
  let injectedFrames = new WeakSet();
  let injectedFramesAdd = WeakSet.prototype.add.bind(injectedFrames);
  let injectedFramesHas = WeakSet.prototype.has.bind(injectedFrames);

  function injectIntoContentWindow(contentWindow)
  {
    if (contentWindow && !injectedFramesHas(contentWindow))
    {
      injectedFramesAdd(contentWindow);
      try
      {
        contentWindow[eventName] = checkRequest;
        contentWindow.eval(
          "(" + injectedToString() + ")('" + eventName + "', true);"
        );
        delete contentWindow[eventName];
      }
      catch (e) {}
    }
  }

  for (let element of [HTMLFrameElement, HTMLIFrameElement, HTMLObjectElement])
  {
    let contentDocumentDesc = Object.getOwnPropertyDescriptor(
      element.prototype, "contentDocument"
    );
    let contentWindowDesc = Object.getOwnPropertyDescriptor(
      element.prototype, "contentWindow"
    );

    // Apparently in HTMLObjectElement.prototype.contentWindow does not exist
    // in older versions of Chrome such as 51.
    if (!contentWindowDesc)
      continue;

    let getContentDocument = Function.prototype.call.bind(
      contentDocumentDesc.get
    );
    let getContentWindow = Function.prototype.call.bind(
      contentWindowDesc.get
    );

    contentWindowDesc.get = function()
    {
      let contentWindow = getContentWindow(this);
      injectIntoContentWindow(contentWindow);
      return contentWindow;
    };
    contentDocumentDesc.get = function()
    {
      injectIntoContentWindow(getContentWindow(this));
      return getContentDocument(this);
    };
    Object.defineProperty(element.prototype, "contentWindow",
                          contentWindowDesc);
    Object.defineProperty(element.prototype, "contentDocument",
                          contentDocumentDesc);
  }

  /*
   * RTCPeerConnection wrapper
   *
   * The webRequest API in Chrome does not yet allow the blocking of
   * WebRTC connections.
   * See https://bugs.chromium.org/p/chromium/issues/detail?id=707683
   */
  let RealCustomEvent = window.CustomEvent;

  // If we've been injected into a frame via contentWindow then we can simply
  // grab the copy of checkRequest left for us by the parent document. Otherwise
  // we need to set it up now, along with the event handling functions.
  if (injectedIntoContentWindow)
  {
    checkRequest = window[eventName];
  }
  else
  {
    let addEventListener = document.addEventListener.bind(document);
    let dispatchEvent = document.dispatchEvent.bind(document);
    let removeEventListener = document.removeEventListener.bind(document);
    checkRequest = (url, callback) =>
    {
      let incomingEventName = eventName + "-" + url;

      function listener(event)
      {
        callback(event.detail);
        removeEventListener(incomingEventName, listener);
      }
      addEventListener(incomingEventName, listener);

      dispatchEvent(new RealCustomEvent(eventName, {detail: {url}}));
    };
  }

  // Only to be called before the page's code, not hardened.
  function copyProperties(src, dest, properties)
  {
    for (let name of properties)
    {
      if (Object.prototype.hasOwnProperty.call(src, name))
      {
        Object.defineProperty(dest, name,
                              Object.getOwnPropertyDescriptor(src, name));
      }
    }
  }

  let RealRTCPeerConnection = window.RTCPeerConnection ||
                              window.webkitRTCPeerConnection;

  // Firefox has the option (media.peerconnection.enabled) to disable WebRTC
  // in which case RealRTCPeerConnection is undefined.
  if (typeof RealRTCPeerConnection != "undefined")
  {
    let closeRTCPeerConnection = Function.prototype.call.bind(
      RealRTCPeerConnection.prototype.close
    );
    let RealArray = Array;
    let RealString = String;
    let {create: createObject, defineProperty} = Object;

    let normalizeUrl = url =>
    {
      if (typeof url != "undefined")
        return RealString(url);
    };

    let safeCopyArray = (originalArray, transform) =>
    {
      if (originalArray == null || typeof originalArray != "object")
        return originalArray;

      let safeArray = RealArray(originalArray.length);
      for (let i = 0; i < safeArray.length; i++)
      {
        defineProperty(safeArray, i, {
          configurable: false, enumerable: false, writable: false,
          value: transform(originalArray[i])
        });
      }
      defineProperty(safeArray, "length", {
        configurable: false, enumerable: false, writable: false,
        value: safeArray.length
      });
      return safeArray;
    };

    // It would be much easier to use the .getConfiguration method to obtain
    // the normalized and safe configuration from the RTCPeerConnection
    // instance. Unfortunately its not implemented as of Chrome unstable 59.
    // See https://www.chromestatus.com/feature/5271355306016768
    let protectConfiguration = configuration =>
    {
      if (configuration == null || typeof configuration != "object")
        return configuration;

      let iceServers = safeCopyArray(
        configuration.iceServers,
        iceServer =>
        {
          let {url, urls} = iceServer;

          // RTCPeerConnection doesn't iterate through pseudo Arrays of urls.
          if (typeof urls != "undefined" && !(urls instanceof RealArray))
            urls = [urls];

          return createObject(iceServer, {
            url: {
              configurable: false, enumerable: false, writable: false,
              value: normalizeUrl(url)
            },
            urls: {
              configurable: false, enumerable: false, writable: false,
              value: safeCopyArray(urls, normalizeUrl)
            }
          });
        }
      );

      return createObject(configuration, {
        iceServers: {
          configurable: false, enumerable: false, writable: false,
          value: iceServers
        }
      });
    };

    let checkUrl = (peerconnection, url) =>
    {
      checkRequest(url, blocked =>
      {
        if (blocked)
        {
          // Calling .close() throws if already closed.
          try
          {
            closeRTCPeerConnection(peerconnection);
          }
          catch (e) {}
        }
      });
    };

    let checkConfiguration = (peerconnection, configuration) =>
    {
      if (configuration && configuration.iceServers)
      {
        for (let i = 0; i < configuration.iceServers.length; i++)
        {
          let iceServer = configuration.iceServers[i];
          if (iceServer)
          {
            if (iceServer.url)
              checkUrl(peerconnection, iceServer.url);

            if (iceServer.urls)
            {
              for (let j = 0; j < iceServer.urls.length; j++)
                checkUrl(peerconnection, iceServer.urls[j]);
            }
          }
        }
      }
    };

    // Chrome unstable (tested with 59) has already implemented
    // setConfiguration, so we need to wrap that if it exists too.
    // https://www.chromestatus.com/feature/5596193748942848
    if (RealRTCPeerConnection.prototype.setConfiguration)
    {
      let realSetConfiguration = Function.prototype.call.bind(
        RealRTCPeerConnection.prototype.setConfiguration
      );

      RealRTCPeerConnection.prototype.setConfiguration = function(configuration)
      {
        configuration = protectConfiguration(configuration);

        // Call the real method first, so that validates the configuration for
        // us. Also we might as well since checkRequest is asynchronous anyway.
        realSetConfiguration(this, configuration);
        checkConfiguration(this, configuration);
      };
    }

    let WrappedRTCPeerConnection = function(...args)
    {
      if (!(this instanceof WrappedRTCPeerConnection))
        return RealRTCPeerConnection();

      let configuration = protectConfiguration(args[0]);

      // Since the old webkitRTCPeerConnection constructor takes an optional
      // second argument we need to take care to pass that through. Necessary
      // for older versions of Chrome such as 51.
      let constraints;
      if (args.length > 1)
        constraints = args[1];

      let peerconnection = new RealRTCPeerConnection(configuration,
                                                     constraints);
      checkConfiguration(peerconnection, configuration);
      return peerconnection;
    };

    WrappedRTCPeerConnection.prototype = RealRTCPeerConnection.prototype;

    let boundWrappedRTCPeerConnection = WrappedRTCPeerConnection.bind();
    copyProperties(RealRTCPeerConnection, boundWrappedRTCPeerConnection,
                   ["generateCertificate", "name", "prototype"]);
    RealRTCPeerConnection.prototype.constructor = boundWrappedRTCPeerConnection;

    if ("RTCPeerConnection" in window)
      window.RTCPeerConnection = boundWrappedRTCPeerConnection;
    if ("webkitRTCPeerConnection" in window)
      window.webkitRTCPeerConnection = boundWrappedRTCPeerConnection;
  }
}

if (document instanceof HTMLDocument)
{
  let sandbox = window.frameElement &&
                window.frameElement.getAttribute("sandbox");

  if (typeof sandbox != "string" || /(^|\s)allow-scripts(\s|$)/i.test(sandbox))
  {
    let script = document.createElement("script");
    let code = "(" + injected + ")('" + randomEventName + "');";

    script.type = "application/javascript";
    script.async = false;

    // Firefox 58 only bypasses site CSPs when assigning to 'src',
    // while Chrome 67 only bypass site CSPs when using 'textContent'.
    if (browser.runtime.getURL("").startsWith("moz-extension://"))
    {
      let url = URL.createObjectURL(new Blob([code]));
      script.src = url;
      document.documentElement.appendChild(script);
      URL.revokeObjectURL(url);
    }
    else
    {
      script.textContent = code;
      document.documentElement.appendChild(script);
    }

    document.documentElement.removeChild(script);
  }
}


/***/ })

/******/ 	});
/************************************************************************/
/******/ 	// The module cache
/******/ 	var __webpack_module_cache__ = {};
/******/ 	
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/ 		// Check if module is in cache
/******/ 		if(__webpack_module_cache__[moduleId]) {
/******/ 			return __webpack_module_cache__[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = __webpack_module_cache__[moduleId] = {
/******/ 			// no module.id needed
/******/ 			// no module.loaded needed
/******/ 			exports: {}
/******/ 		};
/******/ 	
/******/ 		// Execute the module function
/******/ 		__webpack_modules__[moduleId](module, module.exports, __webpack_require__);
/******/ 	
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/ 	
/************************************************************************/
/******/ 	/* webpack/runtime/define property getters */
/******/ 	(() => {
/******/ 		// define getter functions for harmony exports
/******/ 		__webpack_require__.d = (exports, definition) => {
/******/ 			for(var key in definition) {
/******/ 				if(__webpack_require__.o(definition, key) && !__webpack_require__.o(exports, key)) {
/******/ 					Object.defineProperty(exports, key, { enumerable: true, get: definition[key] });
/******/ 				}
/******/ 			}
/******/ 		};
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/hasOwnProperty shorthand */
/******/ 	(() => {
/******/ 		__webpack_require__.o = (obj, prop) => Object.prototype.hasOwnProperty.call(obj, prop)
/******/ 	})();
/******/ 	
/************************************************************************/
/******/ 	// startup
/******/ 	// Load entry module
/******/ 	// This entry module is referenced by other modules so it can't be inlined
/******/ 	__webpack_require__(334);
/******/ 	__webpack_require__(487);
/******/ 	__webpack_require__(659);
/******/ })()
;
//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5jbHVkZS5wcmVsb2FkLmpzIiwic291cmNlcyI6WyJ3ZWJwYWNrOi8vYWRibG9ja3BsdXNjaHJvbWUvLi9hZGJsb2NrcGx1c2NvcmUvbGliL2NvbW1vbi5qcyIsIndlYnBhY2s6Ly9hZGJsb2NrcGx1c2Nocm9tZS8uL2FkYmxvY2twbHVzY29yZS9saWIvY29udGVudC9lbGVtSGlkZUVtdWxhdGlvbi5qcyIsIndlYnBhY2s6Ly9hZGJsb2NrcGx1c2Nocm9tZS8uL2NvbXBvc2VyLnByZWxvYWQuanMiLCJ3ZWJwYWNrOi8vYWRibG9ja3BsdXNjaHJvbWUvLi9pbmNsdWRlLnByZWxvYWQuanMiLCJ3ZWJwYWNrOi8vYWRibG9ja3BsdXNjaHJvbWUvLi9pbmplY3QucHJlbG9hZC5qcyIsIndlYnBhY2s6Ly9hZGJsb2NrcGx1c2Nocm9tZS93ZWJwYWNrL2Jvb3RzdHJhcCIsIndlYnBhY2s6Ly9hZGJsb2NrcGx1c2Nocm9tZS93ZWJwYWNrL3J1bnRpbWUvZGVmaW5lIHByb3BlcnR5IGdldHRlcnMiLCJ3ZWJwYWNrOi8vYWRibG9ja3BsdXNjaHJvbWUvd2VicGFjay9ydW50aW1lL2hhc093blByb3BlcnR5IHNob3J0aGFuZCIsIndlYnBhY2s6Ly9hZGJsb2NrcGx1c2Nocm9tZS93ZWJwYWNrL3N0YXJ0dXAiXSwic291cmNlc0NvbnRlbnQiOlsiLypcclxuICogVGhpcyBmaWxlIGlzIHBhcnQgb2YgQWRibG9jayBQbHVzIDxodHRwczovL2FkYmxvY2twbHVzLm9yZy8+LFxyXG4gKiBDb3B5cmlnaHQgKEMpIDIwMDYtcHJlc2VudCBleWVvIEdtYkhcclxuICpcclxuICogQWRibG9jayBQbHVzIGlzIGZyZWUgc29mdHdhcmU6IHlvdSBjYW4gcmVkaXN0cmlidXRlIGl0IGFuZC9vciBtb2RpZnlcclxuICogaXQgdW5kZXIgdGhlIHRlcm1zIG9mIHRoZSBHTlUgR2VuZXJhbCBQdWJsaWMgTGljZW5zZSB2ZXJzaW9uIDMgYXNcclxuICogcHVibGlzaGVkIGJ5IHRoZSBGcmVlIFNvZnR3YXJlIEZvdW5kYXRpb24uXHJcbiAqXHJcbiAqIEFkYmxvY2sgUGx1cyBpcyBkaXN0cmlidXRlZCBpbiB0aGUgaG9wZSB0aGF0IGl0IHdpbGwgYmUgdXNlZnVsLFxyXG4gKiBidXQgV0lUSE9VVCBBTlkgV0FSUkFOVFk7IHdpdGhvdXQgZXZlbiB0aGUgaW1wbGllZCB3YXJyYW50eSBvZlxyXG4gKiBNRVJDSEFOVEFCSUxJVFkgb3IgRklUTkVTUyBGT1IgQSBQQVJUSUNVTEFSIFBVUlBPU0UuICBTZWUgdGhlXHJcbiAqIEdOVSBHZW5lcmFsIFB1YmxpYyBMaWNlbnNlIGZvciBtb3JlIGRldGFpbHMuXHJcbiAqXHJcbiAqIFlvdSBzaG91bGQgaGF2ZSByZWNlaXZlZCBhIGNvcHkgb2YgdGhlIEdOVSBHZW5lcmFsIFB1YmxpYyBMaWNlbnNlXHJcbiAqIGFsb25nIHdpdGggQWRibG9jayBQbHVzLiAgSWYgbm90LCBzZWUgPGh0dHA6Ly93d3cuZ251Lm9yZy9saWNlbnNlcy8+LlxyXG4gKi9cclxuXHJcbi8qKiBAbW9kdWxlICovXHJcblxyXG5cInVzZSBzdHJpY3RcIjtcclxuXHJcbi8qKlxyXG4gKiBDb252ZXJ0cyByYXcgdGV4dCBpbnRvIGEgcmVndWxhciBleHByZXNzaW9uIHN0cmluZ1xyXG4gKiBAcGFyYW0ge3N0cmluZ30gdGV4dCB0aGUgc3RyaW5nIHRvIGNvbnZlcnRcclxuICogQHJldHVybiB7c3RyaW5nfSByZWd1bGFyIGV4cHJlc3Npb24gcmVwcmVzZW50YXRpb24gb2YgdGhlIHRleHRcclxuICogQHBhY2thZ2VcclxuICovXHJcbmV4cG9ydHMudGV4dFRvUmVnRXhwID0gZnVuY3Rpb24gdGV4dFRvUmVnRXhwKHRleHQpXHJcbntcclxuICByZXR1cm4gdGV4dC5yZXBsYWNlKC9bLS9cXFxcXiQqKz8uKCl8W1xcXXt9XS9nLCBcIlxcXFwkJlwiKTtcclxufTtcclxuXHJcbi8qKlxyXG4gKiBDb252ZXJ0cyBmaWx0ZXIgdGV4dCBpbnRvIHJlZ3VsYXIgZXhwcmVzc2lvbiBzdHJpbmdcclxuICogQHBhcmFtIHtzdHJpbmd9IHRleHQgYXMgaW4gRmlsdGVyKClcclxuICogQHJldHVybiB7c3RyaW5nfSByZWd1bGFyIGV4cHJlc3Npb24gcmVwcmVzZW50YXRpb24gb2YgZmlsdGVyIHRleHRcclxuICogQHBhY2thZ2VcclxuICovXHJcbmV4cG9ydHMuZmlsdGVyVG9SZWdFeHAgPSBmdW5jdGlvbiBmaWx0ZXJUb1JlZ0V4cCh0ZXh0KVxyXG57XHJcbiAgLy8gcmVtb3ZlIG11bHRpcGxlIHdpbGRjYXJkc1xyXG4gIHRleHQgPSB0ZXh0LnJlcGxhY2UoL1xcKisvZywgXCIqXCIpO1xyXG5cclxuICAvLyByZW1vdmUgbGVhZGluZyB3aWxkY2FyZFxyXG4gIGlmICh0ZXh0WzBdID09IFwiKlwiKVxyXG4gICAgdGV4dCA9IHRleHQuc3Vic3RyaW5nKDEpO1xyXG5cclxuICAvLyByZW1vdmUgdHJhaWxpbmcgd2lsZGNhcmRcclxuICBpZiAodGV4dFt0ZXh0Lmxlbmd0aCAtIDFdID09IFwiKlwiKVxyXG4gICAgdGV4dCA9IHRleHQuc3Vic3RyaW5nKDAsIHRleHQubGVuZ3RoIC0gMSk7XHJcblxyXG4gIHJldHVybiB0ZXh0XHJcbiAgICAvLyByZW1vdmUgYW5jaG9ycyBmb2xsb3dpbmcgc2VwYXJhdG9yIHBsYWNlaG9sZGVyXHJcbiAgICAucmVwbGFjZSgvXFxeXFx8JC8sIFwiXlwiKVxyXG4gICAgLy8gZXNjYXBlIHNwZWNpYWwgc3ltYm9sc1xyXG4gICAgLnJlcGxhY2UoL1xcVy9nLCBcIlxcXFwkJlwiKVxyXG4gICAgLy8gcmVwbGFjZSB3aWxkY2FyZHMgYnkgLipcclxuICAgIC5yZXBsYWNlKC9cXFxcXFwqL2csIFwiLipcIilcclxuICAgIC8vIHByb2Nlc3Mgc2VwYXJhdG9yIHBsYWNlaG9sZGVycyAoYWxsIEFOU0kgY2hhcmFjdGVycyBidXQgYWxwaGFudW1lcmljXHJcbiAgICAvLyBjaGFyYWN0ZXJzIGFuZCBfJS4tKVxyXG4gICAgLnJlcGxhY2UoL1xcXFxcXF4vZywgXCIoPzpbXFxcXHgwMC1cXFxceDI0XFxcXHgyNi1cXFxceDJDXFxcXHgyRlxcXFx4M0EtXFxcXHg0MFxcXFx4NUItXFxcXHg1RVxcXFx4NjBcXFxceDdCLVxcXFx4N0ZdfCQpXCIpXHJcbiAgICAvLyBwcm9jZXNzIGV4dGVuZGVkIGFuY2hvciBhdCBleHByZXNzaW9uIHN0YXJ0XHJcbiAgICAucmVwbGFjZSgvXlxcXFxcXHxcXFxcXFx8LywgXCJeW1xcXFx3XFxcXC1dKzpcXFxcLysoPyFcXFxcLykoPzpbXlxcXFwvXStcXFxcLik/XCIpXHJcbiAgICAvLyBwcm9jZXNzIGFuY2hvciBhdCBleHByZXNzaW9uIHN0YXJ0XHJcbiAgICAucmVwbGFjZSgvXlxcXFxcXHwvLCBcIl5cIilcclxuICAgIC8vIHByb2Nlc3MgYW5jaG9yIGF0IGV4cHJlc3Npb24gZW5kXHJcbiAgICAucmVwbGFjZSgvXFxcXFxcfCQvLCBcIiRcIik7XHJcbn07XHJcblxyXG5sZXQgc3BsaXRTZWxlY3RvciA9IGV4cG9ydHMuc3BsaXRTZWxlY3RvciA9IGZ1bmN0aW9uIHNwbGl0U2VsZWN0b3Ioc2VsZWN0b3IpXHJcbntcclxuICBpZiAoIXNlbGVjdG9yLmluY2x1ZGVzKFwiLFwiKSlcclxuICAgIHJldHVybiBbc2VsZWN0b3JdO1xyXG5cclxuICBsZXQgc2VsZWN0b3JzID0gW107XHJcbiAgbGV0IHN0YXJ0ID0gMDtcclxuICBsZXQgbGV2ZWwgPSAwO1xyXG4gIGxldCBzZXAgPSBcIlwiO1xyXG5cclxuICBmb3IgKGxldCBpID0gMDsgaSA8IHNlbGVjdG9yLmxlbmd0aDsgaSsrKVxyXG4gIHtcclxuICAgIGxldCBjaHIgPSBzZWxlY3RvcltpXTtcclxuXHJcbiAgICBpZiAoY2hyID09IFwiXFxcXFwiKSAgICAgICAgLy8gaWdub3JlIGVzY2FwZWQgY2hhcmFjdGVyc1xyXG4gICAge1xyXG4gICAgICBpKys7XHJcbiAgICB9XHJcbiAgICBlbHNlIGlmIChjaHIgPT0gc2VwKSAgICAvLyBkb24ndCBzcGxpdCB3aXRoaW4gcXVvdGVkIHRleHRcclxuICAgIHtcclxuICAgICAgc2VwID0gXCJcIjsgICAgICAgICAgICAgLy8gZS5nLiBbYXR0cj1cIixcIl1cclxuICAgIH1cclxuICAgIGVsc2UgaWYgKHNlcCA9PSBcIlwiKVxyXG4gICAge1xyXG4gICAgICBpZiAoY2hyID09ICdcIicgfHwgY2hyID09IFwiJ1wiKVxyXG4gICAgICB7XHJcbiAgICAgICAgc2VwID0gY2hyO1xyXG4gICAgICB9XHJcbiAgICAgIGVsc2UgaWYgKGNociA9PSBcIihcIikgIC8vIGRvbid0IHNwbGl0IGJldHdlZW4gcGFyZW50aGVzZXNcclxuICAgICAge1xyXG4gICAgICAgIGxldmVsKys7ICAgICAgICAgICAgLy8gZS5nLiA6bWF0Y2hlcyhkaXYsc3BhbilcclxuICAgICAgfVxyXG4gICAgICBlbHNlIGlmIChjaHIgPT0gXCIpXCIpXHJcbiAgICAgIHtcclxuICAgICAgICBsZXZlbCA9IE1hdGgubWF4KDAsIGxldmVsIC0gMSk7XHJcbiAgICAgIH1cclxuICAgICAgZWxzZSBpZiAoY2hyID09IFwiLFwiICYmIGxldmVsID09IDApXHJcbiAgICAgIHtcclxuICAgICAgICBzZWxlY3RvcnMucHVzaChzZWxlY3Rvci5zdWJzdHJpbmcoc3RhcnQsIGkpKTtcclxuICAgICAgICBzdGFydCA9IGkgKyAxO1xyXG4gICAgICB9XHJcbiAgICB9XHJcbiAgfVxyXG5cclxuICBzZWxlY3RvcnMucHVzaChzZWxlY3Rvci5zdWJzdHJpbmcoc3RhcnQpKTtcclxuICByZXR1cm4gc2VsZWN0b3JzO1xyXG59O1xyXG5cclxuZnVuY3Rpb24gZmluZFRhcmdldFNlbGVjdG9ySW5kZXgoc2VsZWN0b3IpXHJcbntcclxuICBsZXQgaW5kZXggPSAwO1xyXG4gIGxldCB3aGl0ZXNwYWNlID0gMDtcclxuICBsZXQgc2NvcGUgPSBbXTtcclxuXHJcbiAgLy8gU3RhcnQgZnJvbSB0aGUgZW5kIG9mIHRoZSBzdHJpbmcgYW5kIGdvIGNoYXJhY3RlciBieSBjaGFyYWN0ZXIsIHdoZXJlIGVhY2hcclxuICAvLyBjaGFyYWN0ZXIgaXMgYSBVbmljb2RlIGNvZGUgcG9pbnQuXHJcbiAgZm9yIChsZXQgY2hhcmFjdGVyIG9mIFsuLi5zZWxlY3Rvcl0ucmV2ZXJzZSgpKVxyXG4gIHtcclxuICAgIGxldCBjdXJyZW50U2NvcGUgPSBzY29wZVtzY29wZS5sZW5ndGggLSAxXTtcclxuXHJcbiAgICBpZiAoY2hhcmFjdGVyID09IFwiJ1wiIHx8IGNoYXJhY3RlciA9PSBcIlxcXCJcIilcclxuICAgIHtcclxuICAgICAgLy8gSWYgd2UncmUgYWxyZWFkeSB3aXRoaW4gdGhlIHNhbWUgdHlwZSBvZiBxdW90ZSwgY2xvc2UgdGhlIHNjb3BlO1xyXG4gICAgICAvLyBvdGhlcndpc2Ugb3BlbiBhIG5ldyBzY29wZS5cclxuICAgICAgaWYgKGN1cnJlbnRTY29wZSA9PSBjaGFyYWN0ZXIpXHJcbiAgICAgICAgc2NvcGUucG9wKCk7XHJcbiAgICAgIGVsc2VcclxuICAgICAgICBzY29wZS5wdXNoKGNoYXJhY3Rlcik7XHJcbiAgICB9XHJcbiAgICBlbHNlIGlmIChjaGFyYWN0ZXIgPT0gXCJdXCIgfHwgY2hhcmFjdGVyID09IFwiKVwiKVxyXG4gICAge1xyXG4gICAgICAvLyBGb3IgY2xvc2luZyBicmFja2V0cyBhbmQgcGFyZW50aGVzZXMsIG9wZW4gYSBuZXcgc2NvcGUgb25seSBpZiB3ZSdyZVxyXG4gICAgICAvLyBub3Qgd2l0aGluIGEgcXVvdGUuIFdpdGhpbiBxdW90ZXMgdGhlc2UgY2hhcmFjdGVycyBzaG91bGQgaGF2ZSBub1xyXG4gICAgICAvLyBtZWFuaW5nLlxyXG4gICAgICBpZiAoY3VycmVudFNjb3BlICE9IFwiJ1wiICYmIGN1cnJlbnRTY29wZSAhPSBcIlxcXCJcIilcclxuICAgICAgICBzY29wZS5wdXNoKGNoYXJhY3Rlcik7XHJcbiAgICB9XHJcbiAgICBlbHNlIGlmIChjaGFyYWN0ZXIgPT0gXCJbXCIpXHJcbiAgICB7XHJcbiAgICAgIC8vIElmIHdlJ3JlIGFscmVhZHkgd2l0aGluIGEgYnJhY2tldCwgY2xvc2UgdGhlIHNjb3BlLlxyXG4gICAgICBpZiAoY3VycmVudFNjb3BlID09IFwiXVwiKVxyXG4gICAgICAgIHNjb3BlLnBvcCgpO1xyXG4gICAgfVxyXG4gICAgZWxzZSBpZiAoY2hhcmFjdGVyID09IFwiKFwiKVxyXG4gICAge1xyXG4gICAgICAvLyBJZiB3ZSdyZSBhbHJlYWR5IHdpdGhpbiBhIHBhcmVudGhlc2lzLCBjbG9zZSB0aGUgc2NvcGUuXHJcbiAgICAgIGlmIChjdXJyZW50U2NvcGUgPT0gXCIpXCIpXHJcbiAgICAgICAgc2NvcGUucG9wKCk7XHJcbiAgICB9XHJcbiAgICBlbHNlIGlmICghY3VycmVudFNjb3BlKVxyXG4gICAge1xyXG4gICAgICAvLyBBdCB0aGUgdG9wIGxldmVsIChub3Qgd2l0aGluIGFueSBzY29wZSksIGNvdW50IHRoZSB3aGl0ZXNwYWNlIGlmIHdlJ3ZlXHJcbiAgICAgIC8vIGVuY291bnRlcmVkIGl0LiBPdGhlcndpc2UgaWYgd2UndmUgaGl0IG9uZSBvZiB0aGUgY29tYmluYXRvcnMsXHJcbiAgICAgIC8vIHRlcm1pbmF0ZSBoZXJlOyBvdGhlcndpc2UgaWYgd2UndmUgaGl0IGEgbm9uLWNvbG9uIGNoYXJhY3RlcixcclxuICAgICAgLy8gdGVybWluYXRlIGhlcmUuXHJcbiAgICAgIGlmICgvXFxzLy50ZXN0KGNoYXJhY3RlcikpXHJcbiAgICAgICAgd2hpdGVzcGFjZSsrO1xyXG4gICAgICBlbHNlIGlmICgoY2hhcmFjdGVyID09IFwiPlwiIHx8IGNoYXJhY3RlciA9PSBcIitcIiB8fCBjaGFyYWN0ZXIgPT0gXCJ+XCIpIHx8XHJcbiAgICAgICAgICAgICAgICh3aGl0ZXNwYWNlID4gMCAmJiBjaGFyYWN0ZXIgIT0gXCI6XCIpKVxyXG4gICAgICAgIGJyZWFrO1xyXG4gICAgfVxyXG5cclxuICAgIC8vIFplcm8gb3V0IHRoZSB3aGl0ZXNwYWNlIGNvdW50IGlmIHdlJ3ZlIGVudGVyZWQgYSBzY29wZS5cclxuICAgIGlmIChzY29wZS5sZW5ndGggPiAwKVxyXG4gICAgICB3aGl0ZXNwYWNlID0gMDtcclxuXHJcbiAgICAvLyBJbmNyZW1lbnQgdGhlIGluZGV4IGJ5IHRoZSBzaXplIG9mIHRoZSBjaGFyYWN0ZXIuIE5vdGUgdGhhdCBmb3IgVW5pY29kZVxyXG4gICAgLy8gY29tcG9zaXRlIGNoYXJhY3RlcnMgKGxpa2UgZW1vamkpIHRoaXMgd2lsbCBiZSBtb3JlIHRoYW4gb25lLlxyXG4gICAgaW5kZXggKz0gY2hhcmFjdGVyLmxlbmd0aDtcclxuICB9XHJcblxyXG4gIHJldHVybiBzZWxlY3Rvci5sZW5ndGggLSBpbmRleCArIHdoaXRlc3BhY2U7XHJcbn1cclxuXHJcbi8qKlxyXG4gKiBRdWFsaWZpZXMgYSBDU1Mgc2VsZWN0b3Igd2l0aCBhIHF1YWxpZmllciwgd2hpY2ggbWF5IGJlIGFub3RoZXIgQ1NTIHNlbGVjdG9yXHJcbiAqIG9yIGFuIGVtcHR5IHN0cmluZy4gRm9yIGV4YW1wbGUsIGdpdmVuIHRoZSBzZWxlY3RvciBcImRpdi5iYXJcIiBhbmQgdGhlXHJcbiAqIHF1YWxpZmllciBcIiNmb29cIiwgdGhpcyBmdW5jdGlvbiByZXR1cm5zIFwiZGl2I2Zvby5iYXJcIi5cclxuICogQHBhcmFtIHtzdHJpbmd9IHNlbGVjdG9yIFRoZSBzZWxlY3RvciB0byBxdWFsaWZ5LlxyXG4gKiBAcGFyYW0ge3N0cmluZ30gcXVhbGlmaWVyIFRoZSBxdWFsaWZpZXIgd2l0aCB3aGljaCB0byBxdWFsaWZ5IHRoZSBzZWxlY3Rvci5cclxuICogQHJldHVybnMge3N0cmluZ30gVGhlIHF1YWxpZmllZCBzZWxlY3Rvci5cclxuICogQHBhY2thZ2VcclxuICovXHJcbmV4cG9ydHMucXVhbGlmeVNlbGVjdG9yID0gZnVuY3Rpb24gcXVhbGlmeVNlbGVjdG9yKHNlbGVjdG9yLCBxdWFsaWZpZXIpXHJcbntcclxuICBsZXQgcXVhbGlmaWVkU2VsZWN0b3IgPSBcIlwiO1xyXG5cclxuICBsZXQgcXVhbGlmaWVyVGFyZ2V0U2VsZWN0b3JJbmRleCA9IGZpbmRUYXJnZXRTZWxlY3RvckluZGV4KHF1YWxpZmllcik7XHJcbiAgbGV0IFssIHF1YWxpZmllclR5cGUgPSBcIlwiXSA9XHJcbiAgICAvXihbYS16XVthLXotXSopPy9pLmV4ZWMocXVhbGlmaWVyLnN1YnN0cmluZyhxdWFsaWZpZXJUYXJnZXRTZWxlY3RvckluZGV4KSk7XHJcblxyXG4gIGZvciAobGV0IHN1YiBvZiBzcGxpdFNlbGVjdG9yKHNlbGVjdG9yKSlcclxuICB7XHJcbiAgICBzdWIgPSBzdWIudHJpbSgpO1xyXG5cclxuICAgIHF1YWxpZmllZFNlbGVjdG9yICs9IFwiLCBcIjtcclxuXHJcbiAgICBsZXQgaW5kZXggPSBmaW5kVGFyZ2V0U2VsZWN0b3JJbmRleChzdWIpO1xyXG5cclxuICAgIC8vIE5vdGUgdGhhdCB0aGUgZmlyc3QgZ3JvdXAgaW4gdGhlIHJlZ3VsYXIgZXhwcmVzc2lvbiBpcyBvcHRpb25hbC4gSWYgaXRcclxuICAgIC8vIGRvZXNuJ3QgbWF0Y2ggKGUuZy4gXCIjZm9vOjpudGgtY2hpbGQoMSlcIiksIHR5cGUgd2lsbCBiZSBhbiBlbXB0eSBzdHJpbmcuXHJcbiAgICBsZXQgWywgdHlwZSA9IFwiXCIsIHJlc3RdID1cclxuICAgICAgL14oW2Etel1bYS16LV0qKT9cXCo/KC4qKS9pLmV4ZWMoc3ViLnN1YnN0cmluZyhpbmRleCkpO1xyXG5cclxuICAgIGlmICh0eXBlID09IHF1YWxpZmllclR5cGUpXHJcbiAgICAgIHR5cGUgPSBcIlwiO1xyXG5cclxuICAgIC8vIElmIHRoZSBxdWFsaWZpZXIgZW5kcyBpbiBhIGNvbWJpbmF0b3IgKGUuZy4gXCJib2R5ICNmb28+XCIpLCB3ZSBwdXQgdGhlXHJcbiAgICAvLyB0eXBlIGFuZCB0aGUgcmVzdCBvZiB0aGUgc2VsZWN0b3IgYWZ0ZXIgdGhlIHF1YWxpZmllclxyXG4gICAgLy8gKGUuZy4gXCJib2R5ICNmb28+ZGl2LmJhclwiKTsgb3RoZXJ3aXNlIChlLmcuIFwiYm9keSAjZm9vXCIpIHdlIG1lcmdlIHRoZVxyXG4gICAgLy8gdHlwZSBpbnRvIHRoZSBxdWFsaWZpZXIgKGUuZy4gXCJib2R5IGRpdiNmb28uYmFyXCIpLlxyXG4gICAgaWYgKC9bXFxzPit+XSQvLnRlc3QocXVhbGlmaWVyKSlcclxuICAgICAgcXVhbGlmaWVkU2VsZWN0b3IgKz0gc3ViLnN1YnN0cmluZygwLCBpbmRleCkgKyBxdWFsaWZpZXIgKyB0eXBlICsgcmVzdDtcclxuICAgIGVsc2VcclxuICAgICAgcXVhbGlmaWVkU2VsZWN0b3IgKz0gc3ViLnN1YnN0cmluZygwLCBpbmRleCkgKyB0eXBlICsgcXVhbGlmaWVyICsgcmVzdDtcclxuICB9XHJcblxyXG4gIC8vIFJlbW92ZSB0aGUgaW5pdGlhbCBjb21tYSBhbmQgc3BhY2UuXHJcbiAgcmV0dXJuIHF1YWxpZmllZFNlbGVjdG9yLnN1YnN0cmluZygyKTtcclxufTtcclxuIiwiLypcclxuICogVGhpcyBmaWxlIGlzIHBhcnQgb2YgQWRibG9jayBQbHVzIDxodHRwczovL2FkYmxvY2twbHVzLm9yZy8+LFxyXG4gKiBDb3B5cmlnaHQgKEMpIDIwMDYtcHJlc2VudCBleWVvIEdtYkhcclxuICpcclxuICogQWRibG9jayBQbHVzIGlzIGZyZWUgc29mdHdhcmU6IHlvdSBjYW4gcmVkaXN0cmlidXRlIGl0IGFuZC9vciBtb2RpZnlcclxuICogaXQgdW5kZXIgdGhlIHRlcm1zIG9mIHRoZSBHTlUgR2VuZXJhbCBQdWJsaWMgTGljZW5zZSB2ZXJzaW9uIDMgYXNcclxuICogcHVibGlzaGVkIGJ5IHRoZSBGcmVlIFNvZnR3YXJlIEZvdW5kYXRpb24uXHJcbiAqXHJcbiAqIEFkYmxvY2sgUGx1cyBpcyBkaXN0cmlidXRlZCBpbiB0aGUgaG9wZSB0aGF0IGl0IHdpbGwgYmUgdXNlZnVsLFxyXG4gKiBidXQgV0lUSE9VVCBBTlkgV0FSUkFOVFk7IHdpdGhvdXQgZXZlbiB0aGUgaW1wbGllZCB3YXJyYW50eSBvZlxyXG4gKiBNRVJDSEFOVEFCSUxJVFkgb3IgRklUTkVTUyBGT1IgQSBQQVJUSUNVTEFSIFBVUlBPU0UuICBTZWUgdGhlXHJcbiAqIEdOVSBHZW5lcmFsIFB1YmxpYyBMaWNlbnNlIGZvciBtb3JlIGRldGFpbHMuXHJcbiAqXHJcbiAqIFlvdSBzaG91bGQgaGF2ZSByZWNlaXZlZCBhIGNvcHkgb2YgdGhlIEdOVSBHZW5lcmFsIFB1YmxpYyBMaWNlbnNlXHJcbiAqIGFsb25nIHdpdGggQWRibG9jayBQbHVzLiAgSWYgbm90LCBzZWUgPGh0dHA6Ly93d3cuZ251Lm9yZy9saWNlbnNlcy8+LlxyXG4gKi9cclxuXHJcbi8qKiBAbW9kdWxlICovXHJcblxyXG5cInVzZSBzdHJpY3RcIjtcclxuXHJcbmNvbnN0IHt0ZXh0VG9SZWdFeHAsIGZpbHRlclRvUmVnRXhwLCBzcGxpdFNlbGVjdG9yLFxyXG4gICAgICAgcXVhbGlmeVNlbGVjdG9yfSA9IHJlcXVpcmUoXCIuLi9jb21tb25cIik7XHJcblxyXG5jb25zdCBNSU5fSU5WT0NBVElPTl9JTlRFUlZBTCA9IDMwMDA7XHJcbmNvbnN0IE1BWF9TWU5DSFJPTk9VU19QUk9DRVNTSU5HX1RJTUUgPSA1MDtcclxuXHJcbmxldCBhYnBTZWxlY3RvclJlZ2V4cCA9IC86LWFicC0oW1xcdy1dKylcXCgvaTtcclxuXHJcbmxldCB0ZXN0SW5mbyA9IG51bGw7XHJcblxyXG5mdW5jdGlvbiB0b0NTU1N0eWxlRGVjbGFyYXRpb24odmFsdWUpXHJcbntcclxuICByZXR1cm4gT2JqZWN0LmFzc2lnbihkb2N1bWVudC5jcmVhdGVFbGVtZW50KFwidGVzdFwiKSwge3N0eWxlOiB2YWx1ZX0pLnN0eWxlO1xyXG59XHJcblxyXG5leHBvcnRzLnNldFRlc3RNb2RlID0gZnVuY3Rpb24gc2V0VGVzdE1vZGUoKVxyXG57XHJcbiAgdGVzdEluZm8gPSB7XHJcbiAgICBsYXN0UHJvY2Vzc2VkRWxlbWVudHM6IG5ldyBTZXQoKVxyXG4gIH07XHJcbn07XHJcblxyXG5leHBvcnRzLmdldFRlc3RJbmZvID0gZnVuY3Rpb24gZ2V0VGVzdEluZm8oKVxyXG57XHJcbiAgcmV0dXJuIHRlc3RJbmZvO1xyXG59O1xyXG5cclxuZnVuY3Rpb24gZ2V0Q2FjaGVkUHJvcGVydHlWYWx1ZShvYmplY3QsIG5hbWUsIGRlZmF1bHRWYWx1ZUZ1bmMgPSAoKSA9PiB7fSlcclxue1xyXG4gIGxldCB2YWx1ZSA9IG9iamVjdFtuYW1lXTtcclxuICBpZiAodHlwZW9mIHZhbHVlID09IFwidW5kZWZpbmVkXCIpXHJcbiAgICBPYmplY3QuZGVmaW5lUHJvcGVydHkob2JqZWN0LCBuYW1lLCB7dmFsdWU6IHZhbHVlID0gZGVmYXVsdFZhbHVlRnVuYygpfSk7XHJcbiAgcmV0dXJuIHZhbHVlO1xyXG59XHJcblxyXG4vKipcclxuICogUmV0dXJuIHBvc2l0aW9uIG9mIG5vZGUgZnJvbSBwYXJlbnQuXHJcbiAqIEBwYXJhbSB7Tm9kZX0gbm9kZSB0aGUgbm9kZSB0byBmaW5kIHRoZSBwb3NpdGlvbiBvZi5cclxuICogQHJldHVybiB7bnVtYmVyfSBPbmUtYmFzZWQgaW5kZXggbGlrZSBmb3IgOm50aC1jaGlsZCgpLCBvciAwIG9uIGVycm9yLlxyXG4gKi9cclxuZnVuY3Rpb24gcG9zaXRpb25JblBhcmVudChub2RlKVxyXG57XHJcbiAgbGV0IGluZGV4ID0gMDtcclxuICBmb3IgKGxldCBjaGlsZCBvZiBub2RlLnBhcmVudE5vZGUuY2hpbGRyZW4pXHJcbiAge1xyXG4gICAgaWYgKGNoaWxkID09IG5vZGUpXHJcbiAgICAgIHJldHVybiBpbmRleCArIDE7XHJcblxyXG4gICAgaW5kZXgrKztcclxuICB9XHJcblxyXG4gIHJldHVybiAwO1xyXG59XHJcblxyXG5mdW5jdGlvbiBtYWtlU2VsZWN0b3Iobm9kZSwgc2VsZWN0b3IgPSBcIlwiKVxyXG57XHJcbiAgaWYgKG5vZGUgPT0gbnVsbClcclxuICAgIHJldHVybiBudWxsO1xyXG4gIGlmICghbm9kZS5wYXJlbnRFbGVtZW50KVxyXG4gIHtcclxuICAgIGxldCBuZXdTZWxlY3RvciA9IFwiOnJvb3RcIjtcclxuICAgIGlmIChzZWxlY3RvcilcclxuICAgICAgbmV3U2VsZWN0b3IgKz0gXCIgPiBcIiArIHNlbGVjdG9yO1xyXG4gICAgcmV0dXJuIG5ld1NlbGVjdG9yO1xyXG4gIH1cclxuICBsZXQgaWR4ID0gcG9zaXRpb25JblBhcmVudChub2RlKTtcclxuICBpZiAoaWR4ID4gMClcclxuICB7XHJcbiAgICBsZXQgbmV3U2VsZWN0b3IgPSBgJHtub2RlLnRhZ05hbWV9Om50aC1jaGlsZCgke2lkeH0pYDtcclxuICAgIGlmIChzZWxlY3RvcilcclxuICAgICAgbmV3U2VsZWN0b3IgKz0gXCIgPiBcIiArIHNlbGVjdG9yO1xyXG4gICAgcmV0dXJuIG1ha2VTZWxlY3Rvcihub2RlLnBhcmVudEVsZW1lbnQsIG5ld1NlbGVjdG9yKTtcclxuICB9XHJcblxyXG4gIHJldHVybiBzZWxlY3RvcjtcclxufVxyXG5cclxuZnVuY3Rpb24gcGFyc2VTZWxlY3RvckNvbnRlbnQoY29udGVudCwgc3RhcnRJbmRleClcclxue1xyXG4gIGxldCBwYXJlbnMgPSAxO1xyXG4gIGxldCBxdW90ZSA9IG51bGw7XHJcbiAgbGV0IGkgPSBzdGFydEluZGV4O1xyXG4gIGZvciAoOyBpIDwgY29udGVudC5sZW5ndGg7IGkrKylcclxuICB7XHJcbiAgICBsZXQgYyA9IGNvbnRlbnRbaV07XHJcbiAgICBpZiAoYyA9PSBcIlxcXFxcIilcclxuICAgIHtcclxuICAgICAgLy8gSWdub3JlIGVzY2FwZWQgY2hhcmFjdGVyc1xyXG4gICAgICBpKys7XHJcbiAgICB9XHJcbiAgICBlbHNlIGlmIChxdW90ZSlcclxuICAgIHtcclxuICAgICAgaWYgKGMgPT0gcXVvdGUpXHJcbiAgICAgICAgcXVvdGUgPSBudWxsO1xyXG4gICAgfVxyXG4gICAgZWxzZSBpZiAoYyA9PSBcIidcIiB8fCBjID09ICdcIicpXHJcbiAgICB7XHJcbiAgICAgIHF1b3RlID0gYztcclxuICAgIH1cclxuICAgIGVsc2UgaWYgKGMgPT0gXCIoXCIpXHJcbiAgICB7XHJcbiAgICAgIHBhcmVucysrO1xyXG4gICAgfVxyXG4gICAgZWxzZSBpZiAoYyA9PSBcIilcIilcclxuICAgIHtcclxuICAgICAgcGFyZW5zLS07XHJcbiAgICAgIGlmIChwYXJlbnMgPT0gMClcclxuICAgICAgICBicmVhaztcclxuICAgIH1cclxuICB9XHJcblxyXG4gIGlmIChwYXJlbnMgPiAwKVxyXG4gICAgcmV0dXJuIG51bGw7XHJcbiAgcmV0dXJuIHt0ZXh0OiBjb250ZW50LnN1YnN0cmluZyhzdGFydEluZGV4LCBpKSwgZW5kOiBpfTtcclxufVxyXG5cclxuLyoqXHJcbiAqIFN0cmluZ2lmaWVkIHN0eWxlIG9iamVjdHNcclxuICogQHR5cGVkZWYge09iamVjdH0gU3RyaW5naWZpZWRTdHlsZVxyXG4gKiBAcHJvcGVydHkge3N0cmluZ30gc3R5bGUgQ1NTIHN0eWxlIHJlcHJlc2VudGVkIGJ5IGEgc3RyaW5nLlxyXG4gKiBAcHJvcGVydHkge3N0cmluZ1tdfSBzdWJTZWxlY3RvcnMgc2VsZWN0b3JzIHRoZSBDU1MgcHJvcGVydGllcyBhcHBseSB0by5cclxuICovXHJcblxyXG4vKipcclxuICogUHJvZHVjZSBhIHN0cmluZyByZXByZXNlbnRhdGlvbiBvZiB0aGUgc3R5bGVzaGVldCBlbnRyeS5cclxuICogQHBhcmFtIHtDU1NTdHlsZVJ1bGV9IHJ1bGUgdGhlIENTUyBzdHlsZSBydWxlLlxyXG4gKiBAcmV0dXJuIHtTdHJpbmdpZmllZFN0eWxlfSB0aGUgc3RyaW5naWZpZWQgc3R5bGUuXHJcbiAqL1xyXG5mdW5jdGlvbiBzdHJpbmdpZnlTdHlsZShydWxlKVxyXG57XHJcbiAgbGV0IHN0eWxlcyA9IFtdO1xyXG4gIGZvciAobGV0IGkgPSAwOyBpIDwgcnVsZS5zdHlsZS5sZW5ndGg7IGkrKylcclxuICB7XHJcbiAgICBsZXQgcHJvcGVydHkgPSBydWxlLnN0eWxlLml0ZW0oaSk7XHJcbiAgICBsZXQgdmFsdWUgPSBydWxlLnN0eWxlLmdldFByb3BlcnR5VmFsdWUocHJvcGVydHkpO1xyXG4gICAgbGV0IHByaW9yaXR5ID0gcnVsZS5zdHlsZS5nZXRQcm9wZXJ0eVByaW9yaXR5KHByb3BlcnR5KTtcclxuICAgIHN0eWxlcy5wdXNoKGAke3Byb3BlcnR5fTogJHt2YWx1ZX0ke3ByaW9yaXR5ID8gXCIgIVwiICsgcHJpb3JpdHkgOiBcIlwifTtgKTtcclxuICB9XHJcbiAgc3R5bGVzLnNvcnQoKTtcclxuICByZXR1cm4ge1xyXG4gICAgc3R5bGU6IHN0eWxlcy5qb2luKFwiIFwiKSxcclxuICAgIHN1YlNlbGVjdG9yczogc3BsaXRTZWxlY3RvcihydWxlLnNlbGVjdG9yVGV4dClcclxuICB9O1xyXG59XHJcblxyXG5sZXQgc2NvcGVTdXBwb3J0ZWQgPSBudWxsO1xyXG5cclxuZnVuY3Rpb24gdHJ5UXVlcnlTZWxlY3RvcihzdWJ0cmVlLCBzZWxlY3RvciwgYWxsKVxyXG57XHJcbiAgbGV0IGVsZW1lbnRzID0gbnVsbDtcclxuICB0cnlcclxuICB7XHJcbiAgICBlbGVtZW50cyA9IGFsbCA/IHN1YnRyZWUucXVlcnlTZWxlY3RvckFsbChzZWxlY3RvcikgOlxyXG4gICAgICBzdWJ0cmVlLnF1ZXJ5U2VsZWN0b3Ioc2VsZWN0b3IpO1xyXG4gICAgc2NvcGVTdXBwb3J0ZWQgPSB0cnVlO1xyXG4gIH1cclxuICBjYXRjaCAoZSlcclxuICB7XHJcbiAgICAvLyBFZGdlIGRvZXNuJ3Qgc3VwcG9ydCBcIjpzY29wZVwiXHJcbiAgICBzY29wZVN1cHBvcnRlZCA9IGZhbHNlO1xyXG4gIH1cclxuICByZXR1cm4gZWxlbWVudHM7XHJcbn1cclxuXHJcbi8qKlxyXG4gKiBRdWVyeSBzZWxlY3Rvci5cclxuICpcclxuICogSWYgaXQgaXMgcmVsYXRpdmUsIHdpbGwgdHJ5IDpzY29wZS5cclxuICpcclxuICogQHBhcmFtIHtOb2RlfSBzdWJ0cmVlIHRoZSBlbGVtZW50IHRvIHF1ZXJ5IHNlbGVjdG9yXHJcbiAqIEBwYXJhbSB7c3RyaW5nfSBzZWxlY3RvciB0aGUgc2VsZWN0b3IgdG8gcXVlcnlcclxuICogQHBhcmFtIHtib29sfSBbYWxsPWZhbHNlXSB0cnVlIHRvIHBlcmZvcm0gcXVlcnlTZWxlY3RvckFsbCgpXHJcbiAqXHJcbiAqIEByZXR1cm5zIHs/KE5vZGV8Tm9kZUxpc3QpfSByZXN1bHQgb2YgdGhlIHF1ZXJ5LiBudWxsIGluIGNhc2Ugb2YgZXJyb3IuXHJcbiAqL1xyXG5mdW5jdGlvbiBzY29wZWRRdWVyeVNlbGVjdG9yKHN1YnRyZWUsIHNlbGVjdG9yLCBhbGwpXHJcbntcclxuICBpZiAoc2VsZWN0b3JbMF0gPT0gXCI+XCIpXHJcbiAge1xyXG4gICAgc2VsZWN0b3IgPSBcIjpzY29wZVwiICsgc2VsZWN0b3I7XHJcbiAgICBpZiAoc2NvcGVTdXBwb3J0ZWQpXHJcbiAgICB7XHJcbiAgICAgIHJldHVybiBhbGwgPyBzdWJ0cmVlLnF1ZXJ5U2VsZWN0b3JBbGwoc2VsZWN0b3IpIDpcclxuICAgICAgICBzdWJ0cmVlLnF1ZXJ5U2VsZWN0b3Ioc2VsZWN0b3IpO1xyXG4gICAgfVxyXG4gICAgaWYgKHNjb3BlU3VwcG9ydGVkID09IG51bGwpXHJcbiAgICAgIHJldHVybiB0cnlRdWVyeVNlbGVjdG9yKHN1YnRyZWUsIHNlbGVjdG9yLCBhbGwpO1xyXG4gICAgcmV0dXJuIG51bGw7XHJcbiAgfVxyXG4gIHJldHVybiBhbGwgPyBzdWJ0cmVlLnF1ZXJ5U2VsZWN0b3JBbGwoc2VsZWN0b3IpIDpcclxuICAgIHN1YnRyZWUucXVlcnlTZWxlY3RvcihzZWxlY3Rvcik7XHJcbn1cclxuXHJcbmZ1bmN0aW9uIHNjb3BlZFF1ZXJ5U2VsZWN0b3JBbGwoc3VidHJlZSwgc2VsZWN0b3IpXHJcbntcclxuICByZXR1cm4gc2NvcGVkUXVlcnlTZWxlY3RvcihzdWJ0cmVlLCBzZWxlY3RvciwgdHJ1ZSk7XHJcbn1cclxuXHJcbmNvbnN0IHJlZ2V4cFJlZ2V4cCA9IC9eXFwvKC4qKVxcLyhbaW11XSopJC87XHJcblxyXG4vKipcclxuICogTWFrZSBhIHJlZ3VsYXIgZXhwcmVzc2lvbiBmcm9tIGEgdGV4dCBhcmd1bWVudC5cclxuICpcclxuICogSWYgaXQgY2FuIGJlIHBhcnNlZCBhcyBhIHJlZ3VsYXIgZXhwcmVzc2lvbiwgcGFyc2UgaXQgYW5kIHRoZSBmbGFncy5cclxuICpcclxuICogQHBhcmFtIHtzdHJpbmd9IHRleHQgdGhlIHRleHQgYXJndW1lbnQuXHJcbiAqXHJcbiAqIEByZXR1cm4gez9SZWdFeHB9IGEgUmVnRXhwIG9iamVjdCBvciBudWxsIGluIGNhc2Ugb2YgZXJyb3IuXHJcbiAqL1xyXG5mdW5jdGlvbiBtYWtlUmVnRXhwUGFyYW1ldGVyKHRleHQpXHJcbntcclxuICBsZXQgWywgcGF0dGVybiwgZmxhZ3NdID1cclxuICAgICAgcmVnZXhwUmVnZXhwLmV4ZWModGV4dCkgfHwgW251bGwsIHRleHRUb1JlZ0V4cCh0ZXh0KV07XHJcblxyXG4gIHRyeVxyXG4gIHtcclxuICAgIHJldHVybiBuZXcgUmVnRXhwKHBhdHRlcm4sIGZsYWdzKTtcclxuICB9XHJcbiAgY2F0Y2ggKGUpXHJcbiAge1xyXG4gIH1cclxuICByZXR1cm4gbnVsbDtcclxufVxyXG5cclxuZnVuY3Rpb24qIGV2YWx1YXRlKGNoYWluLCBpbmRleCwgcHJlZml4LCBzdWJ0cmVlLCBzdHlsZXMsIHRhcmdldHMpXHJcbntcclxuICBpZiAoaW5kZXggPj0gY2hhaW4ubGVuZ3RoKVxyXG4gIHtcclxuICAgIHlpZWxkIHByZWZpeDtcclxuICAgIHJldHVybjtcclxuICB9XHJcbiAgZm9yIChsZXQgW3NlbGVjdG9yLCBlbGVtZW50XSBvZiBjaGFpbltpbmRleF0uZ2V0U2VsZWN0b3JzKHByZWZpeCwgc3VidHJlZSxcclxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc3R5bGVzLCB0YXJnZXRzKSlcclxuICB7XHJcbiAgICBpZiAoc2VsZWN0b3IgPT0gbnVsbClcclxuICAgICAgeWllbGQgbnVsbDtcclxuICAgIGVsc2VcclxuICAgICAgeWllbGQqIGV2YWx1YXRlKGNoYWluLCBpbmRleCArIDEsIHNlbGVjdG9yLCBlbGVtZW50LCBzdHlsZXMsIHRhcmdldHMpO1xyXG4gIH1cclxuICAvLyBKdXN0IGluIGNhc2UgdGhlIGdldFNlbGVjdG9ycygpIGdlbmVyYXRvciBhYm92ZSBoYWQgdG8gcnVuIHNvbWUgaGVhdnlcclxuICAvLyBkb2N1bWVudC5xdWVyeVNlbGVjdG9yQWxsKCkgY2FsbCB3aGljaCBkaWRuJ3QgcHJvZHVjZSBhbnkgcmVzdWx0cywgbWFrZVxyXG4gIC8vIHN1cmUgdGhlcmUgaXMgYXQgbGVhc3Qgb25lIHBvaW50IHdoZXJlIGV4ZWN1dGlvbiBjYW4gcGF1c2UuXHJcbiAgeWllbGQgbnVsbDtcclxufVxyXG5cclxuY2xhc3MgUGxhaW5TZWxlY3RvclxyXG57XHJcbiAgY29uc3RydWN0b3Ioc2VsZWN0b3IpXHJcbiAge1xyXG4gICAgdGhpcy5fc2VsZWN0b3IgPSBzZWxlY3RvcjtcclxuICAgIHRoaXMubWF5YmVEZXBlbmRzT25BdHRyaWJ1dGVzID0gL1sjLl18XFxbLitcXF0vLnRlc3Qoc2VsZWN0b3IpO1xyXG4gICAgdGhpcy5kZXBlbmRzT25ET00gPSB0aGlzLm1heWJlRGVwZW5kc09uQXR0cmlidXRlcztcclxuICAgIHRoaXMubWF5YmVDb250YWluc1NpYmxpbmdDb21iaW5hdG9ycyA9IC9bfitdLy50ZXN0KHNlbGVjdG9yKTtcclxuICB9XHJcblxyXG4gIC8qKlxyXG4gICAqIEdlbmVyYXRvciBmdW5jdGlvbiByZXR1cm5pbmcgYSBwYWlyIG9mIHNlbGVjdG9yIHN0cmluZyBhbmQgc3VidHJlZS5cclxuICAgKiBAcGFyYW0ge3N0cmluZ30gcHJlZml4IHRoZSBwcmVmaXggZm9yIHRoZSBzZWxlY3Rvci5cclxuICAgKiBAcGFyYW0ge05vZGV9IHN1YnRyZWUgdGhlIHN1YnRyZWUgd2Ugd29yayBvbi5cclxuICAgKiBAcGFyYW0ge1N0cmluZ2lmaWVkU3R5bGVbXX0gc3R5bGVzIHRoZSBzdHJpbmdpZmllZCBzdHlsZSBvYmplY3RzLlxyXG4gICAqIEBwYXJhbSB7Tm9kZVtdfSBbdGFyZ2V0c10gdGhlIG5vZGVzIHdlIGFyZSBpbnRlcmVzdGVkIGluLlxyXG4gICAqL1xyXG4gICpnZXRTZWxlY3RvcnMocHJlZml4LCBzdWJ0cmVlLCBzdHlsZXMsIHRhcmdldHMpXHJcbiAge1xyXG4gICAgeWllbGQgW3ByZWZpeCArIHRoaXMuX3NlbGVjdG9yLCBzdWJ0cmVlXTtcclxuICB9XHJcbn1cclxuXHJcbmNvbnN0IGluY29tcGxldGVQcmVmaXhSZWdleHAgPSAvW1xccz4rfl0kLztcclxuXHJcbmNsYXNzIEhhc1NlbGVjdG9yXHJcbntcclxuICBjb25zdHJ1Y3RvcihzZWxlY3RvcnMpXHJcbiAge1xyXG4gICAgdGhpcy5kZXBlbmRzT25ET00gPSB0cnVlO1xyXG5cclxuICAgIHRoaXMuX2lubmVyU2VsZWN0b3JzID0gc2VsZWN0b3JzO1xyXG4gIH1cclxuXHJcbiAgZ2V0IGRlcGVuZHNPblN0eWxlcygpXHJcbiAge1xyXG4gICAgcmV0dXJuIHRoaXMuX2lubmVyU2VsZWN0b3JzLnNvbWUoc2VsZWN0b3IgPT4gc2VsZWN0b3IuZGVwZW5kc09uU3R5bGVzKTtcclxuICB9XHJcblxyXG4gIGdldCBkZXBlbmRzT25DaGFyYWN0ZXJEYXRhKClcclxuICB7XHJcbiAgICByZXR1cm4gdGhpcy5faW5uZXJTZWxlY3RvcnMuc29tZShcclxuICAgICAgc2VsZWN0b3IgPT4gc2VsZWN0b3IuZGVwZW5kc09uQ2hhcmFjdGVyRGF0YVxyXG4gICAgKTtcclxuICB9XHJcblxyXG4gIGdldCBtYXliZURlcGVuZHNPbkF0dHJpYnV0ZXMoKVxyXG4gIHtcclxuICAgIHJldHVybiB0aGlzLl9pbm5lclNlbGVjdG9ycy5zb21lKFxyXG4gICAgICBzZWxlY3RvciA9PiBzZWxlY3Rvci5tYXliZURlcGVuZHNPbkF0dHJpYnV0ZXNcclxuICAgICk7XHJcbiAgfVxyXG5cclxuICAqZ2V0U2VsZWN0b3JzKHByZWZpeCwgc3VidHJlZSwgc3R5bGVzLCB0YXJnZXRzKVxyXG4gIHtcclxuICAgIGZvciAobGV0IGVsZW1lbnQgb2YgdGhpcy5nZXRFbGVtZW50cyhwcmVmaXgsIHN1YnRyZWUsIHN0eWxlcywgdGFyZ2V0cykpXHJcbiAgICAgIHlpZWxkIFttYWtlU2VsZWN0b3IoZWxlbWVudCksIGVsZW1lbnRdO1xyXG4gIH1cclxuXHJcbiAgLyoqXHJcbiAgICogR2VuZXJhdG9yIGZ1bmN0aW9uIHJldHVybmluZyBzZWxlY3RlZCBlbGVtZW50cy5cclxuICAgKiBAcGFyYW0ge3N0cmluZ30gcHJlZml4IHRoZSBwcmVmaXggZm9yIHRoZSBzZWxlY3Rvci5cclxuICAgKiBAcGFyYW0ge05vZGV9IHN1YnRyZWUgdGhlIHN1YnRyZWUgd2Ugd29yayBvbi5cclxuICAgKiBAcGFyYW0ge1N0cmluZ2lmaWVkU3R5bGVbXX0gc3R5bGVzIHRoZSBzdHJpbmdpZmllZCBzdHlsZSBvYmplY3RzLlxyXG4gICAqIEBwYXJhbSB7Tm9kZVtdfSBbdGFyZ2V0c10gdGhlIG5vZGVzIHdlIGFyZSBpbnRlcmVzdGVkIGluLlxyXG4gICAqL1xyXG4gICpnZXRFbGVtZW50cyhwcmVmaXgsIHN1YnRyZWUsIHN0eWxlcywgdGFyZ2V0cylcclxuICB7XHJcbiAgICBsZXQgYWN0dWFsUHJlZml4ID0gKCFwcmVmaXggfHwgaW5jb21wbGV0ZVByZWZpeFJlZ2V4cC50ZXN0KHByZWZpeCkpID9cclxuICAgICAgICBwcmVmaXggKyBcIipcIiA6IHByZWZpeDtcclxuICAgIGxldCBlbGVtZW50cyA9IHNjb3BlZFF1ZXJ5U2VsZWN0b3JBbGwoc3VidHJlZSwgYWN0dWFsUHJlZml4KTtcclxuICAgIGlmIChlbGVtZW50cylcclxuICAgIHtcclxuICAgICAgZm9yIChsZXQgZWxlbWVudCBvZiBlbGVtZW50cylcclxuICAgICAge1xyXG4gICAgICAgIC8vIElmIHRoZSBlbGVtZW50IGlzIG5laXRoZXIgYW4gYW5jZXN0b3Igbm9yIGEgZGVzY2VuZGFudCBvZiBvbmUgb2YgdGhlXHJcbiAgICAgICAgLy8gdGFyZ2V0cywgd2UgY2FuIHNraXAgaXQuXHJcbiAgICAgICAgaWYgKHRhcmdldHMgJiYgIXRhcmdldHMuc29tZSh0YXJnZXQgPT4gZWxlbWVudC5jb250YWlucyh0YXJnZXQpIHx8XHJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdGFyZ2V0LmNvbnRhaW5zKGVsZW1lbnQpKSlcclxuICAgICAgICB7XHJcbiAgICAgICAgICB5aWVsZCBudWxsO1xyXG4gICAgICAgICAgY29udGludWU7XHJcbiAgICAgICAgfVxyXG5cclxuICAgICAgICBsZXQgaXRlciA9IGV2YWx1YXRlKHRoaXMuX2lubmVyU2VsZWN0b3JzLCAwLCBcIlwiLCBlbGVtZW50LCBzdHlsZXMsXHJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB0YXJnZXRzKTtcclxuICAgICAgICBmb3IgKGxldCBzZWxlY3RvciBvZiBpdGVyKVxyXG4gICAgICAgIHtcclxuICAgICAgICAgIGlmIChzZWxlY3RvciA9PSBudWxsKVxyXG4gICAgICAgICAgICB5aWVsZCBudWxsO1xyXG4gICAgICAgICAgZWxzZSBpZiAoc2NvcGVkUXVlcnlTZWxlY3RvcihlbGVtZW50LCBzZWxlY3RvcikpXHJcbiAgICAgICAgICAgIHlpZWxkIGVsZW1lbnQ7XHJcbiAgICAgICAgfVxyXG4gICAgICAgIHlpZWxkIG51bGw7XHJcblxyXG4gICAgICAgIGlmICh0ZXN0SW5mbylcclxuICAgICAgICAgIHRlc3RJbmZvLmxhc3RQcm9jZXNzZWRFbGVtZW50cy5hZGQoZWxlbWVudCk7XHJcbiAgICAgIH1cclxuICAgIH1cclxuICB9XHJcbn1cclxuXHJcbmNsYXNzIENvbnRhaW5zU2VsZWN0b3Jcclxue1xyXG4gIGNvbnN0cnVjdG9yKHRleHRDb250ZW50KVxyXG4gIHtcclxuICAgIHRoaXMuZGVwZW5kc09uRE9NID0gdHJ1ZTtcclxuICAgIHRoaXMuZGVwZW5kc09uQ2hhcmFjdGVyRGF0YSA9IHRydWU7XHJcblxyXG4gICAgdGhpcy5fcmVnZXhwID0gbWFrZVJlZ0V4cFBhcmFtZXRlcih0ZXh0Q29udGVudCk7XHJcbiAgfVxyXG5cclxuICAqZ2V0U2VsZWN0b3JzKHByZWZpeCwgc3VidHJlZSwgc3R5bGVzLCB0YXJnZXRzKVxyXG4gIHtcclxuICAgIGZvciAobGV0IGVsZW1lbnQgb2YgdGhpcy5nZXRFbGVtZW50cyhwcmVmaXgsIHN1YnRyZWUsIHN0eWxlcywgdGFyZ2V0cykpXHJcbiAgICAgIHlpZWxkIFttYWtlU2VsZWN0b3IoZWxlbWVudCksIHN1YnRyZWVdO1xyXG4gIH1cclxuXHJcbiAgKmdldEVsZW1lbnRzKHByZWZpeCwgc3VidHJlZSwgc3R5bGVzLCB0YXJnZXRzKVxyXG4gIHtcclxuICAgIGxldCBhY3R1YWxQcmVmaXggPSAoIXByZWZpeCB8fCBpbmNvbXBsZXRlUHJlZml4UmVnZXhwLnRlc3QocHJlZml4KSkgP1xyXG4gICAgICAgIHByZWZpeCArIFwiKlwiIDogcHJlZml4O1xyXG5cclxuICAgIGxldCBlbGVtZW50cyA9IHNjb3BlZFF1ZXJ5U2VsZWN0b3JBbGwoc3VidHJlZSwgYWN0dWFsUHJlZml4KTtcclxuXHJcbiAgICBpZiAoZWxlbWVudHMpXHJcbiAgICB7XHJcbiAgICAgIGxldCBsYXN0Um9vdCA9IG51bGw7XHJcbiAgICAgIGZvciAobGV0IGVsZW1lbnQgb2YgZWxlbWVudHMpXHJcbiAgICAgIHtcclxuICAgICAgICAvLyBGb3IgYSBmaWx0ZXIgbGlrZSBkaXY6LWFicC1jb250YWlucyhIZWxsbykgYW5kIGEgc3VidHJlZSBsaWtlXHJcbiAgICAgICAgLy8gPGRpdiBpZD1cImFcIj48ZGl2IGlkPVwiYlwiPjxkaXYgaWQ9XCJjXCI+SGVsbG88L2Rpdj48L2Rpdj48L2Rpdj5cclxuICAgICAgICAvLyB3ZSdyZSBvbmx5IGludGVyZXN0ZWQgaW4gZGl2I2FcclxuICAgICAgICBpZiAobGFzdFJvb3QgJiYgbGFzdFJvb3QuY29udGFpbnMoZWxlbWVudCkpXHJcbiAgICAgICAge1xyXG4gICAgICAgICAgeWllbGQgbnVsbDtcclxuICAgICAgICAgIGNvbnRpbnVlO1xyXG4gICAgICAgIH1cclxuXHJcbiAgICAgICAgbGFzdFJvb3QgPSBlbGVtZW50O1xyXG5cclxuICAgICAgICBpZiAodGFyZ2V0cyAmJiAhdGFyZ2V0cy5zb21lKHRhcmdldCA9PiBlbGVtZW50LmNvbnRhaW5zKHRhcmdldCkgfHxcclxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB0YXJnZXQuY29udGFpbnMoZWxlbWVudCkpKVxyXG4gICAgICAgIHtcclxuICAgICAgICAgIHlpZWxkIG51bGw7XHJcbiAgICAgICAgICBjb250aW51ZTtcclxuICAgICAgICB9XHJcblxyXG4gICAgICAgIGlmICh0aGlzLl9yZWdleHAgJiYgdGhpcy5fcmVnZXhwLnRlc3QoZWxlbWVudC50ZXh0Q29udGVudCkpXHJcbiAgICAgICAgICB5aWVsZCBlbGVtZW50O1xyXG4gICAgICAgIGVsc2VcclxuICAgICAgICAgIHlpZWxkIG51bGw7XHJcblxyXG4gICAgICAgIGlmICh0ZXN0SW5mbylcclxuICAgICAgICAgIHRlc3RJbmZvLmxhc3RQcm9jZXNzZWRFbGVtZW50cy5hZGQoZWxlbWVudCk7XHJcbiAgICAgIH1cclxuICAgIH1cclxuICB9XHJcbn1cclxuXHJcbmNsYXNzIFByb3BzU2VsZWN0b3Jcclxue1xyXG4gIGNvbnN0cnVjdG9yKHByb3BlcnR5RXhwcmVzc2lvbilcclxuICB7XHJcbiAgICB0aGlzLmRlcGVuZHNPblN0eWxlcyA9IHRydWU7XHJcbiAgICB0aGlzLmRlcGVuZHNPbkRPTSA9IHRydWU7XHJcblxyXG4gICAgbGV0IHJlZ2V4cFN0cmluZztcclxuICAgIGlmIChwcm9wZXJ0eUV4cHJlc3Npb24ubGVuZ3RoID49IDIgJiYgcHJvcGVydHlFeHByZXNzaW9uWzBdID09IFwiL1wiICYmXHJcbiAgICAgICAgcHJvcGVydHlFeHByZXNzaW9uW3Byb3BlcnR5RXhwcmVzc2lvbi5sZW5ndGggLSAxXSA9PSBcIi9cIilcclxuICAgICAgcmVnZXhwU3RyaW5nID0gcHJvcGVydHlFeHByZXNzaW9uLnNsaWNlKDEsIC0xKTtcclxuICAgIGVsc2VcclxuICAgICAgcmVnZXhwU3RyaW5nID0gZmlsdGVyVG9SZWdFeHAocHJvcGVydHlFeHByZXNzaW9uKTtcclxuXHJcbiAgICB0aGlzLl9yZWdleHAgPSBuZXcgUmVnRXhwKHJlZ2V4cFN0cmluZywgXCJpXCIpO1xyXG4gIH1cclxuXHJcbiAgKmZpbmRQcm9wc1NlbGVjdG9ycyhzdHlsZXMsIHByZWZpeCwgcmVnZXhwKVxyXG4gIHtcclxuICAgIGZvciAobGV0IHN0eWxlIG9mIHN0eWxlcylcclxuICAgIHtcclxuICAgICAgaWYgKHJlZ2V4cC50ZXN0KHN0eWxlLnN0eWxlKSlcclxuICAgICAge1xyXG4gICAgICAgIGZvciAobGV0IHN1YlNlbGVjdG9yIG9mIHN0eWxlLnN1YlNlbGVjdG9ycylcclxuICAgICAgICB7XHJcbiAgICAgICAgICBpZiAoc3ViU2VsZWN0b3Iuc3RhcnRzV2l0aChcIipcIikgJiZcclxuICAgICAgICAgICAgICAhaW5jb21wbGV0ZVByZWZpeFJlZ2V4cC50ZXN0KHByZWZpeCkpXHJcbiAgICAgICAgICAgIHN1YlNlbGVjdG9yID0gc3ViU2VsZWN0b3Iuc3Vic3RyaW5nKDEpO1xyXG5cclxuICAgICAgICAgIGxldCBpZHggPSBzdWJTZWxlY3Rvci5sYXN0SW5kZXhPZihcIjo6XCIpO1xyXG4gICAgICAgICAgaWYgKGlkeCAhPSAtMSlcclxuICAgICAgICAgICAgc3ViU2VsZWN0b3IgPSBzdWJTZWxlY3Rvci5zdWJzdHJpbmcoMCwgaWR4KTtcclxuXHJcbiAgICAgICAgICB5aWVsZCBxdWFsaWZ5U2VsZWN0b3Ioc3ViU2VsZWN0b3IsIHByZWZpeCk7XHJcbiAgICAgICAgfVxyXG4gICAgICB9XHJcbiAgICB9XHJcbiAgfVxyXG5cclxuICAqZ2V0U2VsZWN0b3JzKHByZWZpeCwgc3VidHJlZSwgc3R5bGVzLCB0YXJnZXRzKVxyXG4gIHtcclxuICAgIGZvciAobGV0IHNlbGVjdG9yIG9mIHRoaXMuZmluZFByb3BzU2VsZWN0b3JzKHN0eWxlcywgcHJlZml4LCB0aGlzLl9yZWdleHApKVxyXG4gICAgICB5aWVsZCBbc2VsZWN0b3IsIHN1YnRyZWVdO1xyXG4gIH1cclxufVxyXG5cclxuY2xhc3MgUGF0dGVyblxyXG57XHJcbiAgY29uc3RydWN0b3Ioc2VsZWN0b3JzLCB0ZXh0KVxyXG4gIHtcclxuICAgIHRoaXMuc2VsZWN0b3JzID0gc2VsZWN0b3JzO1xyXG4gICAgdGhpcy50ZXh0ID0gdGV4dDtcclxuICB9XHJcblxyXG4gIGdldCBkZXBlbmRzT25TdHlsZXMoKVxyXG4gIHtcclxuICAgIHJldHVybiBnZXRDYWNoZWRQcm9wZXJ0eVZhbHVlKFxyXG4gICAgICB0aGlzLCBcIl9kZXBlbmRzT25TdHlsZXNcIixcclxuICAgICAgKCkgPT4gdGhpcy5zZWxlY3RvcnMuc29tZShzZWxlY3RvciA9PiBzZWxlY3Rvci5kZXBlbmRzT25TdHlsZXMpXHJcbiAgICApO1xyXG4gIH1cclxuXHJcbiAgZ2V0IGRlcGVuZHNPbkRPTSgpXHJcbiAge1xyXG4gICAgcmV0dXJuIGdldENhY2hlZFByb3BlcnR5VmFsdWUoXHJcbiAgICAgIHRoaXMsIFwiX2RlcGVuZHNPbkRPTVwiLFxyXG4gICAgICAoKSA9PiB0aGlzLnNlbGVjdG9ycy5zb21lKHNlbGVjdG9yID0+IHNlbGVjdG9yLmRlcGVuZHNPbkRPTSlcclxuICAgICk7XHJcbiAgfVxyXG5cclxuICBnZXQgZGVwZW5kc09uU3R5bGVzQW5kRE9NKClcclxuICB7XHJcbiAgICByZXR1cm4gZ2V0Q2FjaGVkUHJvcGVydHlWYWx1ZShcclxuICAgICAgdGhpcywgXCJfZGVwZW5kc09uU3R5bGVzQW5kRE9NXCIsXHJcbiAgICAgICgpID0+IHRoaXMuc2VsZWN0b3JzLnNvbWUoc2VsZWN0b3IgPT4gc2VsZWN0b3IuZGVwZW5kc09uU3R5bGVzICYmXHJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgc2VsZWN0b3IuZGVwZW5kc09uRE9NKVxyXG4gICAgKTtcclxuICB9XHJcblxyXG4gIGdldCBtYXliZURlcGVuZHNPbkF0dHJpYnV0ZXMoKVxyXG4gIHtcclxuICAgIC8vIE9ic2VydmUgY2hhbmdlcyB0byBhdHRyaWJ1dGVzIGlmIGVpdGhlciB0aGVyZSdzIGEgcGxhaW4gc2VsZWN0b3IgdGhhdFxyXG4gICAgLy8gbG9va3MgbGlrZSBhbiBJRCBzZWxlY3RvciwgY2xhc3Mgc2VsZWN0b3IsIG9yIGF0dHJpYnV0ZSBzZWxlY3RvciBpbiBvbmVcclxuICAgIC8vIG9mIHRoZSBwYXR0ZXJucyAoZS5nLiBcImFbaHJlZj0naHR0cHM6Ly9leGFtcGxlLmNvbS8nXVwiKVxyXG4gICAgLy8gb3IgdGhlcmUncyBhIHByb3BlcnRpZXMgc2VsZWN0b3IgbmVzdGVkIGluc2lkZSBhIGhhcyBzZWxlY3RvclxyXG4gICAgLy8gKGUuZy4gXCJkaXY6LWFicC1oYXMoOi1hYnAtcHJvcGVydGllcyhjb2xvcjogYmx1ZSkpXCIpXHJcbiAgICByZXR1cm4gZ2V0Q2FjaGVkUHJvcGVydHlWYWx1ZShcclxuICAgICAgdGhpcywgXCJfbWF5YmVEZXBlbmRzT25BdHRyaWJ1dGVzXCIsXHJcbiAgICAgICgpID0+IHRoaXMuc2VsZWN0b3JzLnNvbWUoXHJcbiAgICAgICAgc2VsZWN0b3IgPT4gc2VsZWN0b3IubWF5YmVEZXBlbmRzT25BdHRyaWJ1dGVzIHx8XHJcbiAgICAgICAgICAgICAgICAgICAgKHNlbGVjdG9yIGluc3RhbmNlb2YgSGFzU2VsZWN0b3IgJiZcclxuICAgICAgICAgICAgICAgICAgICAgc2VsZWN0b3IuZGVwZW5kc09uU3R5bGVzKVxyXG4gICAgICApXHJcbiAgICApO1xyXG4gIH1cclxuXHJcbiAgZ2V0IGRlcGVuZHNPbkNoYXJhY3RlckRhdGEoKVxyXG4gIHtcclxuICAgIC8vIE9ic2VydmUgY2hhbmdlcyB0byBjaGFyYWN0ZXIgZGF0YSBvbmx5IGlmIHRoZXJlJ3MgYSBjb250YWlucyBzZWxlY3RvciBpblxyXG4gICAgLy8gb25lIG9mIHRoZSBwYXR0ZXJucy5cclxuICAgIHJldHVybiBnZXRDYWNoZWRQcm9wZXJ0eVZhbHVlKFxyXG4gICAgICB0aGlzLCBcIl9kZXBlbmRzT25DaGFyYWN0ZXJEYXRhXCIsXHJcbiAgICAgICgpID0+IHRoaXMuc2VsZWN0b3JzLnNvbWUoc2VsZWN0b3IgPT4gc2VsZWN0b3IuZGVwZW5kc09uQ2hhcmFjdGVyRGF0YSlcclxuICAgICk7XHJcbiAgfVxyXG5cclxuICBnZXQgbWF5YmVDb250YWluc1NpYmxpbmdDb21iaW5hdG9ycygpXHJcbiAge1xyXG4gICAgcmV0dXJuIGdldENhY2hlZFByb3BlcnR5VmFsdWUoXHJcbiAgICAgIHRoaXMsIFwiX21heWJlQ29udGFpbnNTaWJsaW5nQ29tYmluYXRvcnNcIixcclxuICAgICAgKCkgPT4gdGhpcy5zZWxlY3RvcnMuc29tZShcclxuICAgICAgICBzZWxlY3RvciA9PiBzZWxlY3Rvci5tYXliZUNvbnRhaW5zU2libGluZ0NvbWJpbmF0b3JzXHJcbiAgICAgIClcclxuICAgICk7XHJcbiAgfVxyXG5cclxuICBtYXRjaGVzTXV0YXRpb25UeXBlcyhtdXRhdGlvblR5cGVzKVxyXG4gIHtcclxuICAgIGxldCBtdXRhdGlvblR5cGVNYXRjaE1hcCA9IGdldENhY2hlZFByb3BlcnR5VmFsdWUoXHJcbiAgICAgIHRoaXMsIFwiX211dGF0aW9uVHlwZU1hdGNoTWFwXCIsXHJcbiAgICAgICgpID0+IG5ldyBNYXAoW1xyXG4gICAgICAgIC8vIEFsbCB0eXBlcyBvZiBET00tZGVwZW5kZW50IHBhdHRlcm5zIGFyZSBhZmZlY3RlZCBieSBtdXRhdGlvbnMgb2ZcclxuICAgICAgICAvLyB0eXBlIFwiY2hpbGRMaXN0XCIuXHJcbiAgICAgICAgW1wiY2hpbGRMaXN0XCIsIHRydWVdLFxyXG4gICAgICAgIFtcImF0dHJpYnV0ZXNcIiwgdGhpcy5tYXliZURlcGVuZHNPbkF0dHJpYnV0ZXNdLFxyXG4gICAgICAgIFtcImNoYXJhY3RlckRhdGFcIiwgdGhpcy5kZXBlbmRzT25DaGFyYWN0ZXJEYXRhXVxyXG4gICAgICBdKVxyXG4gICAgKTtcclxuXHJcbiAgICBmb3IgKGxldCBtdXRhdGlvblR5cGUgb2YgbXV0YXRpb25UeXBlcylcclxuICAgIHtcclxuICAgICAgaWYgKG11dGF0aW9uVHlwZU1hdGNoTWFwLmdldChtdXRhdGlvblR5cGUpKVxyXG4gICAgICAgIHJldHVybiB0cnVlO1xyXG4gICAgfVxyXG5cclxuICAgIHJldHVybiBmYWxzZTtcclxuICB9XHJcbn1cclxuXHJcbmZ1bmN0aW9uIGV4dHJhY3RNdXRhdGlvblR5cGVzKG11dGF0aW9ucylcclxue1xyXG4gIGxldCB0eXBlcyA9IG5ldyBTZXQoKTtcclxuXHJcbiAgZm9yIChsZXQgbXV0YXRpb24gb2YgbXV0YXRpb25zKVxyXG4gIHtcclxuICAgIHR5cGVzLmFkZChtdXRhdGlvbi50eXBlKTtcclxuXHJcbiAgICAvLyBUaGVyZSBhcmUgb25seSAzIHR5cGVzIG9mIG11dGF0aW9uczogXCJhdHRyaWJ1dGVzXCIsIFwiY2hhcmFjdGVyRGF0YVwiLCBhbmRcclxuICAgIC8vIFwiY2hpbGRMaXN0XCIuXHJcbiAgICBpZiAodHlwZXMuc2l6ZSA9PSAzKVxyXG4gICAgICBicmVhaztcclxuICB9XHJcblxyXG4gIHJldHVybiB0eXBlcztcclxufVxyXG5cclxuZnVuY3Rpb24gZXh0cmFjdE11dGF0aW9uVGFyZ2V0cyhtdXRhdGlvbnMpXHJcbntcclxuICBpZiAoIW11dGF0aW9ucylcclxuICAgIHJldHVybiBudWxsO1xyXG5cclxuICBsZXQgdGFyZ2V0cyA9IG5ldyBTZXQoKTtcclxuXHJcbiAgZm9yIChsZXQgbXV0YXRpb24gb2YgbXV0YXRpb25zKVxyXG4gIHtcclxuICAgIGlmIChtdXRhdGlvbi50eXBlID09IFwiY2hpbGRMaXN0XCIpXHJcbiAgICB7XHJcbiAgICAgIC8vIFdoZW4gbmV3IG5vZGVzIGFyZSBhZGRlZCwgd2UncmUgaW50ZXJlc3RlZCBpbiB0aGUgYWRkZWQgbm9kZXMgcmF0aGVyXHJcbiAgICAgIC8vIHRoYW4gdGhlIHBhcmVudC5cclxuICAgICAgZm9yIChsZXQgbm9kZSBvZiBtdXRhdGlvbi5hZGRlZE5vZGVzKVxyXG4gICAgICAgIHRhcmdldHMuYWRkKG5vZGUpO1xyXG4gICAgfVxyXG4gICAgZWxzZVxyXG4gICAge1xyXG4gICAgICB0YXJnZXRzLmFkZChtdXRhdGlvbi50YXJnZXQpO1xyXG4gICAgfVxyXG4gIH1cclxuXHJcbiAgcmV0dXJuIFsuLi50YXJnZXRzXTtcclxufVxyXG5cclxuZnVuY3Rpb24gZmlsdGVyUGF0dGVybnMocGF0dGVybnMsIHtzdHlsZXNoZWV0cywgbXV0YXRpb25zfSlcclxue1xyXG4gIGlmICghc3R5bGVzaGVldHMgJiYgIW11dGF0aW9ucylcclxuICAgIHJldHVybiBwYXR0ZXJucy5zbGljZSgpO1xyXG5cclxuICBsZXQgbXV0YXRpb25UeXBlcyA9IG11dGF0aW9ucyA/IGV4dHJhY3RNdXRhdGlvblR5cGVzKG11dGF0aW9ucykgOiBudWxsO1xyXG5cclxuICByZXR1cm4gcGF0dGVybnMuZmlsdGVyKFxyXG4gICAgcGF0dGVybiA9PiAoc3R5bGVzaGVldHMgJiYgcGF0dGVybi5kZXBlbmRzT25TdHlsZXMpIHx8XHJcbiAgICAgICAgICAgICAgIChtdXRhdGlvbnMgJiYgcGF0dGVybi5kZXBlbmRzT25ET00gJiZcclxuICAgICAgICAgICAgICAgIHBhdHRlcm4ubWF0Y2hlc011dGF0aW9uVHlwZXMobXV0YXRpb25UeXBlcykpXHJcbiAgKTtcclxufVxyXG5cclxuZnVuY3Rpb24gc2hvdWxkT2JzZXJ2ZUF0dHJpYnV0ZXMocGF0dGVybnMpXHJcbntcclxuICByZXR1cm4gcGF0dGVybnMuc29tZShwYXR0ZXJuID0+IHBhdHRlcm4ubWF5YmVEZXBlbmRzT25BdHRyaWJ1dGVzKTtcclxufVxyXG5cclxuZnVuY3Rpb24gc2hvdWxkT2JzZXJ2ZUNoYXJhY3RlckRhdGEocGF0dGVybnMpXHJcbntcclxuICByZXR1cm4gcGF0dGVybnMuc29tZShwYXR0ZXJuID0+IHBhdHRlcm4uZGVwZW5kc09uQ2hhcmFjdGVyRGF0YSk7XHJcbn1cclxuXHJcbmV4cG9ydHMuRWxlbUhpZGVFbXVsYXRpb24gPSBjbGFzcyBFbGVtSGlkZUVtdWxhdGlvblxyXG57XHJcbiAgY29uc3RydWN0b3IoaGlkZUVsZW1zRnVuYylcclxuICB7XHJcbiAgICB0aGlzLl9taW5JbnZvY2F0aW9uSW50ZXJ2YWwgPSBNSU5fSU5WT0NBVElPTl9JTlRFUlZBTDtcclxuICAgIHRoaXMuX2ZpbHRlcmluZ0luUHJvZ3Jlc3MgPSBmYWxzZTtcclxuICAgIHRoaXMuX2xhc3RJbnZvY2F0aW9uID0gLU1JTl9JTlZPQ0FUSU9OX0lOVEVSVkFMO1xyXG4gICAgdGhpcy5fc2NoZWR1bGVkUHJvY2Vzc2luZyA9IG51bGw7XHJcblxyXG4gICAgdGhpcy5kb2N1bWVudCA9IGRvY3VtZW50O1xyXG4gICAgdGhpcy5oaWRlRWxlbXNGdW5jID0gaGlkZUVsZW1zRnVuYztcclxuICAgIHRoaXMub2JzZXJ2ZXIgPSBuZXcgTXV0YXRpb25PYnNlcnZlcih0aGlzLm9ic2VydmUuYmluZCh0aGlzKSk7XHJcbiAgfVxyXG5cclxuICBpc1NhbWVPcmlnaW4oc3R5bGVzaGVldClcclxuICB7XHJcbiAgICB0cnlcclxuICAgIHtcclxuICAgICAgcmV0dXJuIG5ldyBVUkwoc3R5bGVzaGVldC5ocmVmKS5vcmlnaW4gPT0gdGhpcy5kb2N1bWVudC5sb2NhdGlvbi5vcmlnaW47XHJcbiAgICB9XHJcbiAgICBjYXRjaCAoZSlcclxuICAgIHtcclxuICAgICAgLy8gSW52YWxpZCBVUkwsIGFzc3VtZSB0aGF0IGl0IGlzIGZpcnN0LXBhcnR5LlxyXG4gICAgICByZXR1cm4gdHJ1ZTtcclxuICAgIH1cclxuICB9XHJcblxyXG4gIC8qKlxyXG4gICAqIFBhcnNlIHRoZSBzZWxlY3RvclxyXG4gICAqIEBwYXJhbSB7c3RyaW5nfSBzZWxlY3RvciB0aGUgc2VsZWN0b3IgdG8gcGFyc2VcclxuICAgKiBAcmV0dXJuIHtBcnJheX0gc2VsZWN0b3JzIGlzIGFuIGFycmF5IG9mIG9iamVjdHMsXHJcbiAgICogb3IgbnVsbCBpbiBjYXNlIG9mIGVycm9ycy5cclxuICAgKi9cclxuICBwYXJzZVNlbGVjdG9yKHNlbGVjdG9yKVxyXG4gIHtcclxuICAgIGlmIChzZWxlY3Rvci5sZW5ndGggPT0gMClcclxuICAgICAgcmV0dXJuIFtdO1xyXG5cclxuICAgIGxldCBtYXRjaCA9IGFicFNlbGVjdG9yUmVnZXhwLmV4ZWMoc2VsZWN0b3IpO1xyXG4gICAgaWYgKCFtYXRjaClcclxuICAgICAgcmV0dXJuIFtuZXcgUGxhaW5TZWxlY3RvcihzZWxlY3RvcildO1xyXG5cclxuICAgIGxldCBzZWxlY3RvcnMgPSBbXTtcclxuICAgIGlmIChtYXRjaC5pbmRleCA+IDApXHJcbiAgICAgIHNlbGVjdG9ycy5wdXNoKG5ldyBQbGFpblNlbGVjdG9yKHNlbGVjdG9yLnN1YnN0cmluZygwLCBtYXRjaC5pbmRleCkpKTtcclxuXHJcbiAgICBsZXQgc3RhcnRJbmRleCA9IG1hdGNoLmluZGV4ICsgbWF0Y2hbMF0ubGVuZ3RoO1xyXG4gICAgbGV0IGNvbnRlbnQgPSBwYXJzZVNlbGVjdG9yQ29udGVudChzZWxlY3Rvciwgc3RhcnRJbmRleCk7XHJcbiAgICBpZiAoIWNvbnRlbnQpXHJcbiAgICB7XHJcbiAgICAgIGNvbnNvbGUud2FybihuZXcgU3ludGF4RXJyb3IoXCJGYWlsZWQgdG8gcGFyc2UgQWRibG9jayBQbHVzIFwiICtcclxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBgc2VsZWN0b3IgJHtzZWxlY3Rvcn0gYCArXHJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJkdWUgdG8gdW5tYXRjaGVkIHBhcmVudGhlc2VzLlwiKSk7XHJcbiAgICAgIHJldHVybiBudWxsO1xyXG4gICAgfVxyXG4gICAgaWYgKG1hdGNoWzFdID09IFwicHJvcGVydGllc1wiKVxyXG4gICAge1xyXG4gICAgICBzZWxlY3RvcnMucHVzaChuZXcgUHJvcHNTZWxlY3Rvcihjb250ZW50LnRleHQpKTtcclxuICAgIH1cclxuICAgIGVsc2UgaWYgKG1hdGNoWzFdID09IFwiaGFzXCIpXHJcbiAgICB7XHJcbiAgICAgIGxldCBoYXNTZWxlY3RvcnMgPSB0aGlzLnBhcnNlU2VsZWN0b3IoY29udGVudC50ZXh0KTtcclxuICAgICAgaWYgKGhhc1NlbGVjdG9ycyA9PSBudWxsKVxyXG4gICAgICAgIHJldHVybiBudWxsO1xyXG4gICAgICBzZWxlY3RvcnMucHVzaChuZXcgSGFzU2VsZWN0b3IoaGFzU2VsZWN0b3JzKSk7XHJcbiAgICB9XHJcbiAgICBlbHNlIGlmIChtYXRjaFsxXSA9PSBcImNvbnRhaW5zXCIpXHJcbiAgICB7XHJcbiAgICAgIHNlbGVjdG9ycy5wdXNoKG5ldyBDb250YWluc1NlbGVjdG9yKGNvbnRlbnQudGV4dCkpO1xyXG4gICAgfVxyXG4gICAgZWxzZVxyXG4gICAge1xyXG4gICAgICAvLyB0aGlzIGlzIGFuIGVycm9yLCBjYW4ndCBwYXJzZSBzZWxlY3Rvci5cclxuICAgICAgY29uc29sZS53YXJuKG5ldyBTeW50YXhFcnJvcihcIkZhaWxlZCB0byBwYXJzZSBBZGJsb2NrIFBsdXMgXCIgK1xyXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGBzZWxlY3RvciAke3NlbGVjdG9yfSwgaW52YWxpZCBgICtcclxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBgcHNldWRvLWNsYXNzIDotYWJwLSR7bWF0Y2hbMV19KCkuYCkpO1xyXG4gICAgICByZXR1cm4gbnVsbDtcclxuICAgIH1cclxuXHJcbiAgICBsZXQgc3VmZml4ID0gdGhpcy5wYXJzZVNlbGVjdG9yKHNlbGVjdG9yLnN1YnN0cmluZyhjb250ZW50LmVuZCArIDEpKTtcclxuICAgIGlmIChzdWZmaXggPT0gbnVsbClcclxuICAgICAgcmV0dXJuIG51bGw7XHJcblxyXG4gICAgc2VsZWN0b3JzLnB1c2goLi4uc3VmZml4KTtcclxuXHJcbiAgICBpZiAoc2VsZWN0b3JzLmxlbmd0aCA9PSAxICYmIHNlbGVjdG9yc1swXSBpbnN0YW5jZW9mIENvbnRhaW5zU2VsZWN0b3IpXHJcbiAgICB7XHJcbiAgICAgIGNvbnNvbGUud2FybihuZXcgU3ludGF4RXJyb3IoXCJGYWlsZWQgdG8gcGFyc2UgQWRibG9jayBQbHVzIFwiICtcclxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBgc2VsZWN0b3IgJHtzZWxlY3Rvcn0sIGNhbid0IGAgK1xyXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiaGF2ZSBhIGxvbmVseSA6LWFicC1jb250YWlucygpLlwiKSk7XHJcbiAgICAgIHJldHVybiBudWxsO1xyXG4gICAgfVxyXG4gICAgcmV0dXJuIHNlbGVjdG9ycztcclxuICB9XHJcblxyXG4gIC8qKlxyXG4gICAqIFByb2Nlc3NlcyB0aGUgY3VycmVudCBkb2N1bWVudCBhbmQgYXBwbGllcyBhbGwgcnVsZXMgdG8gaXQuXHJcbiAgICogQHBhcmFtIHtDU1NTdHlsZVNoZWV0W119IFtzdHlsZXNoZWV0c11cclxuICAgKiAgICBUaGUgbGlzdCBvZiBuZXcgc3R5bGVzaGVldHMgdGhhdCBoYXZlIGJlZW4gYWRkZWQgdG8gdGhlIGRvY3VtZW50IGFuZFxyXG4gICAqICAgIG1hZGUgcmVwcm9jZXNzaW5nIG5lY2Vzc2FyeS4gVGhpcyBwYXJhbWV0ZXIgc2hvdWxkbid0IGJlIHBhc3NlZCBpbiBmb3JcclxuICAgKiAgICB0aGUgaW5pdGlhbCBwcm9jZXNzaW5nLCBhbGwgb2YgZG9jdW1lbnQncyBzdHlsZXNoZWV0cyB3aWxsIGJlIGNvbnNpZGVyZWRcclxuICAgKiAgICB0aGVuIGFuZCBhbGwgcnVsZXMsIGluY2x1ZGluZyB0aGUgb25lcyBub3QgZGVwZW5kZW50IG9uIHN0eWxlcy5cclxuICAgKiBAcGFyYW0ge011dGF0aW9uUmVjb3JkW119IFttdXRhdGlvbnNdXHJcbiAgICogICAgVGhlIGxpc3Qgb2YgRE9NIG11dGF0aW9ucyB0aGF0IGhhdmUgYmVlbiBhcHBsaWVkIHRvIHRoZSBkb2N1bWVudCBhbmRcclxuICAgKiAgICBtYWRlIHJlcHJvY2Vzc2luZyBuZWNlc3NhcnkuIFRoaXMgcGFyYW1ldGVyIHNob3VsZG4ndCBiZSBwYXNzZWQgaW4gZm9yXHJcbiAgICogICAgdGhlIGluaXRpYWwgcHJvY2Vzc2luZywgdGhlIGVudGlyZSBkb2N1bWVudCB3aWxsIGJlIGNvbnNpZGVyZWRcclxuICAgKiAgICB0aGVuIGFuZCBhbGwgcnVsZXMsIGluY2x1ZGluZyB0aGUgb25lcyBub3QgZGVwZW5kZW50IG9uIHRoZSBET00uXHJcbiAgICogQHBhcmFtIHtmdW5jdGlvbn0gW2RvbmVdXHJcbiAgICogICAgQ2FsbGJhY2sgdG8gY2FsbCB3aGVuIGRvbmUuXHJcbiAgICovXHJcbiAgX2FkZFNlbGVjdG9ycyhzdHlsZXNoZWV0cywgbXV0YXRpb25zLCBkb25lKVxyXG4gIHtcclxuICAgIGlmICh0ZXN0SW5mbylcclxuICAgICAgdGVzdEluZm8ubGFzdFByb2Nlc3NlZEVsZW1lbnRzLmNsZWFyKCk7XHJcblxyXG4gICAgbGV0IHBhdHRlcm5zID0gZmlsdGVyUGF0dGVybnModGhpcy5wYXR0ZXJucywge3N0eWxlc2hlZXRzLCBtdXRhdGlvbnN9KTtcclxuXHJcbiAgICBsZXQgZWxlbWVudHMgPSBbXTtcclxuICAgIGxldCBlbGVtZW50RmlsdGVycyA9IFtdO1xyXG5cclxuICAgIGxldCBjc3NTdHlsZXMgPSBbXTtcclxuXHJcbiAgICAvLyBJZiBuZWl0aGVyIGFueSBzdHlsZSBzaGVldHMgbm9yIGFueSBET00gbXV0YXRpb25zIGhhdmUgYmVlbiBzcGVjaWZpZWQsXHJcbiAgICAvLyBkbyBmdWxsIHByb2Nlc3NpbmcuXHJcbiAgICBpZiAoIXN0eWxlc2hlZXRzICYmICFtdXRhdGlvbnMpXHJcbiAgICAgIHN0eWxlc2hlZXRzID0gdGhpcy5kb2N1bWVudC5zdHlsZVNoZWV0cztcclxuXHJcbiAgICAvLyBJZiB0aGVyZSBhcmUgYW55IERPTSBtdXRhdGlvbnMgYW5kIGFueSBvZiB0aGUgcGF0dGVybnMgZGVwZW5kcyBvbiBib3RoXHJcbiAgICAvLyBzdHlsZSBzaGVldHMgYW5kIHRoZSBET00gKGUuZy4gLWFicC1oYXMoLWFicC1wcm9wZXJ0aWVzKSksIGZpbmQgYWxsIHRoZVxyXG4gICAgLy8gcnVsZXMgaW4gZXZlcnkgc3R5bGUgc2hlZXQgaW4gdGhlIGRvY3VtZW50LCBiZWNhdXNlIHdlIG5lZWQgdG8gcnVuXHJcbiAgICAvLyBxdWVyeVNlbGVjdG9yQWxsIGFmdGVyd2FyZHMuIE9uIHRoZSBvdGhlciBoYW5kLCBpZiB3ZSBvbmx5IGhhdmUgcGF0dGVybnNcclxuICAgIC8vIHRoYXQgZGVwZW5kIG9uIGVpdGhlciBzdHlsZXMgb3IgRE9NIGJvdGggbm90IGJvdGggKGUuZy4gLWFicC1jb250YWlucyksXHJcbiAgICAvLyB3ZSBjYW4gc2tpcCB0aGlzIHBhcnQuXHJcbiAgICBpZiAobXV0YXRpb25zICYmIHBhdHRlcm5zLnNvbWUocGF0dGVybiA9PiBwYXR0ZXJuLmRlcGVuZHNPblN0eWxlc0FuZERPTSkpXHJcbiAgICAgIHN0eWxlc2hlZXRzID0gdGhpcy5kb2N1bWVudC5zdHlsZVNoZWV0cztcclxuXHJcbiAgICBmb3IgKGxldCBzdHlsZXNoZWV0IG9mIHN0eWxlc2hlZXRzIHx8IFtdKVxyXG4gICAge1xyXG4gICAgICAvLyBFeHBsaWNpdGx5IGlnbm9yZSB0aGlyZC1wYXJ0eSBzdHlsZXNoZWV0cyB0byBlbnN1cmUgY29uc2lzdGVudCBiZWhhdmlvclxyXG4gICAgICAvLyBiZXR3ZWVuIEZpcmVmb3ggYW5kIENocm9tZS5cclxuICAgICAgaWYgKCF0aGlzLmlzU2FtZU9yaWdpbihzdHlsZXNoZWV0KSlcclxuICAgICAgICBjb250aW51ZTtcclxuXHJcbiAgICAgIGxldCBydWxlcztcclxuICAgICAgdHJ5XHJcbiAgICAgIHtcclxuICAgICAgICBydWxlcyA9IHN0eWxlc2hlZXQuY3NzUnVsZXM7XHJcbiAgICAgIH1cclxuICAgICAgY2F0Y2ggKGUpXHJcbiAgICAgIHtcclxuICAgICAgICAvLyBPbiBGaXJlZm94LCB0aGVyZSBpcyBhIGNoYW5jZSB0aGF0IGFuIEludmFsaWRBY2Nlc3NFcnJvclxyXG4gICAgICAgIC8vIGdldCB0aHJvd24gd2hlbiBhY2Nlc3NpbmcgY3NzUnVsZXMuIEp1c3Qgc2tpcCB0aGUgc3R5bGVzaGVldFxyXG4gICAgICAgIC8vIGluIHRoYXQgY2FzZS5cclxuICAgICAgICAvLyBTZWUgaHR0cHM6Ly9zZWFyY2hmb3gub3JnL21vemlsbGEtY2VudHJhbC9yZXYvZjY1ZDc1MjhlMzRlZjFhNzY2NWI0YTFhN2I3Y2RiMTM4OGZjZDNhYS9sYXlvdXQvc3R5bGUvU3R5bGVTaGVldC5jcHAjNjk5XHJcbiAgICAgICAgY29udGludWU7XHJcbiAgICAgIH1cclxuXHJcbiAgICAgIGlmICghcnVsZXMpXHJcbiAgICAgICAgY29udGludWU7XHJcblxyXG4gICAgICBmb3IgKGxldCBydWxlIG9mIHJ1bGVzKVxyXG4gICAgICB7XHJcbiAgICAgICAgaWYgKHJ1bGUudHlwZSAhPSBydWxlLlNUWUxFX1JVTEUpXHJcbiAgICAgICAgICBjb250aW51ZTtcclxuXHJcbiAgICAgICAgY3NzU3R5bGVzLnB1c2goc3RyaW5naWZ5U3R5bGUocnVsZSkpO1xyXG4gICAgICB9XHJcbiAgICB9XHJcblxyXG4gICAgbGV0IHRhcmdldHMgPSBleHRyYWN0TXV0YXRpb25UYXJnZXRzKG11dGF0aW9ucyk7XHJcblxyXG4gICAgbGV0IHBhdHRlcm4gPSBudWxsO1xyXG4gICAgbGV0IGdlbmVyYXRvciA9IG51bGw7XHJcblxyXG4gICAgbGV0IHByb2Nlc3NQYXR0ZXJucyA9ICgpID0+XHJcbiAgICB7XHJcbiAgICAgIGxldCBjeWNsZVN0YXJ0ID0gcGVyZm9ybWFuY2Uubm93KCk7XHJcblxyXG4gICAgICBpZiAoIXBhdHRlcm4pXHJcbiAgICAgIHtcclxuICAgICAgICBpZiAoIXBhdHRlcm5zLmxlbmd0aClcclxuICAgICAgICB7XHJcbiAgICAgICAgICBpZiAoZWxlbWVudHMubGVuZ3RoID4gMClcclxuICAgICAgICAgICAgdGhpcy5oaWRlRWxlbXNGdW5jKGVsZW1lbnRzLCBlbGVtZW50RmlsdGVycyk7XHJcbiAgICAgICAgICBpZiAodHlwZW9mIGRvbmUgPT0gXCJmdW5jdGlvblwiKVxyXG4gICAgICAgICAgICBkb25lKCk7XHJcbiAgICAgICAgICByZXR1cm47XHJcbiAgICAgICAgfVxyXG5cclxuICAgICAgICBwYXR0ZXJuID0gcGF0dGVybnMuc2hpZnQoKTtcclxuXHJcbiAgICAgICAgbGV0IGV2YWx1YXRpb25UYXJnZXRzID0gdGFyZ2V0cztcclxuXHJcbiAgICAgICAgLy8gSWYgdGhlIHBhdHRlcm4gYXBwZWFycyB0byBjb250YWluIGFueSBzaWJsaW5nIGNvbWJpbmF0b3JzLCB3ZSBjYW4ndFxyXG4gICAgICAgIC8vIGVhc2lseSBvcHRpbWl6ZSBiYXNlZCBvbiB0aGUgbXV0YXRpb24gdGFyZ2V0cy4gU2luY2UgdGhpcyBpcyBhXHJcbiAgICAgICAgLy8gc3BlY2lhbCBjYXNlLCBza2lwIHRoZSBvcHRpbWl6YXRpb24uIEJ5IHNldHRpbmcgaXQgdG8gbnVsbCBoZXJlIHdlXHJcbiAgICAgICAgLy8gbWFrZSBzdXJlIHdlIHByb2Nlc3MgdGhlIGVudGlyZSBET00uXHJcbiAgICAgICAgaWYgKHBhdHRlcm4ubWF5YmVDb250YWluc1NpYmxpbmdDb21iaW5hdG9ycylcclxuICAgICAgICAgIGV2YWx1YXRpb25UYXJnZXRzID0gbnVsbDtcclxuXHJcbiAgICAgICAgZ2VuZXJhdG9yID0gZXZhbHVhdGUocGF0dGVybi5zZWxlY3RvcnMsIDAsIFwiXCIsXHJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdGhpcy5kb2N1bWVudCwgY3NzU3R5bGVzLCBldmFsdWF0aW9uVGFyZ2V0cyk7XHJcbiAgICAgIH1cclxuICAgICAgZm9yIChsZXQgc2VsZWN0b3Igb2YgZ2VuZXJhdG9yKVxyXG4gICAgICB7XHJcbiAgICAgICAgaWYgKHNlbGVjdG9yICE9IG51bGwpXHJcbiAgICAgICAge1xyXG4gICAgICAgICAgZm9yIChsZXQgZWxlbWVudCBvZiB0aGlzLmRvY3VtZW50LnF1ZXJ5U2VsZWN0b3JBbGwoc2VsZWN0b3IpKVxyXG4gICAgICAgICAge1xyXG4gICAgICAgICAgICBlbGVtZW50cy5wdXNoKGVsZW1lbnQpO1xyXG4gICAgICAgICAgICBlbGVtZW50RmlsdGVycy5wdXNoKHBhdHRlcm4udGV4dCk7XHJcbiAgICAgICAgICB9XHJcbiAgICAgICAgfVxyXG4gICAgICAgIGlmIChwZXJmb3JtYW5jZS5ub3coKSAtIGN5Y2xlU3RhcnQgPiBNQVhfU1lOQ0hST05PVVNfUFJPQ0VTU0lOR19USU1FKVxyXG4gICAgICAgIHtcclxuICAgICAgICAgIHNldFRpbWVvdXQocHJvY2Vzc1BhdHRlcm5zLCAwKTtcclxuICAgICAgICAgIHJldHVybjtcclxuICAgICAgICB9XHJcbiAgICAgIH1cclxuICAgICAgcGF0dGVybiA9IG51bGw7XHJcbiAgICAgIHJldHVybiBwcm9jZXNzUGF0dGVybnMoKTtcclxuICAgIH07XHJcblxyXG4gICAgcHJvY2Vzc1BhdHRlcm5zKCk7XHJcbiAgfVxyXG5cclxuICAvLyBUaGlzIHByb3BlcnR5IGlzIG9ubHkgdXNlZCBpbiB0aGUgdGVzdHNcclxuICAvLyB0byBzaG9ydGVuIHRoZSBpbnZvY2F0aW9uIGludGVydmFsXHJcbiAgZ2V0IG1pbkludm9jYXRpb25JbnRlcnZhbCgpXHJcbiAge1xyXG4gICAgcmV0dXJuIHRoaXMuX21pbkludm9jYXRpb25JbnRlcnZhbDtcclxuICB9XHJcblxyXG4gIHNldCBtaW5JbnZvY2F0aW9uSW50ZXJ2YWwoaW50ZXJ2YWwpXHJcbiAge1xyXG4gICAgdGhpcy5fbWluSW52b2NhdGlvbkludGVydmFsID0gaW50ZXJ2YWw7XHJcbiAgfVxyXG5cclxuICAvKipcclxuICAgKiBSZS1ydW4gZmlsdGVyaW5nIGVpdGhlciBpbW1lZGlhdGVseSBvciBxdWV1ZWQuXHJcbiAgICogQHBhcmFtIHtDU1NTdHlsZVNoZWV0W119IFtzdHlsZXNoZWV0c11cclxuICAgKiAgICBuZXcgc3R5bGVzaGVldHMgdG8gYmUgcHJvY2Vzc2VkLiBUaGlzIHBhcmFtZXRlciBzaG91bGQgYmUgb21pdHRlZFxyXG4gICAqICAgIGZvciBmdWxsIHJlcHJvY2Vzc2luZy5cclxuICAgKiBAcGFyYW0ge011dGF0aW9uUmVjb3JkW119IFttdXRhdGlvbnNdXHJcbiAgICogICAgbmV3IERPTSBtdXRhdGlvbnMgdG8gYmUgcHJvY2Vzc2VkLiBUaGlzIHBhcmFtZXRlciBzaG91bGQgYmUgb21pdHRlZFxyXG4gICAqICAgIGZvciBmdWxsIHJlcHJvY2Vzc2luZy5cclxuICAgKi9cclxuICBxdWV1ZUZpbHRlcmluZyhzdHlsZXNoZWV0cywgbXV0YXRpb25zKVxyXG4gIHtcclxuICAgIGxldCBjb21wbGV0aW9uID0gKCkgPT5cclxuICAgIHtcclxuICAgICAgdGhpcy5fbGFzdEludm9jYXRpb24gPSBwZXJmb3JtYW5jZS5ub3coKTtcclxuICAgICAgdGhpcy5fZmlsdGVyaW5nSW5Qcm9ncmVzcyA9IGZhbHNlO1xyXG4gICAgICBpZiAodGhpcy5fc2NoZWR1bGVkUHJvY2Vzc2luZylcclxuICAgICAge1xyXG4gICAgICAgIGxldCBwYXJhbXMgPSBPYmplY3QuYXNzaWduKHt9LCB0aGlzLl9zY2hlZHVsZWRQcm9jZXNzaW5nKTtcclxuICAgICAgICB0aGlzLl9zY2hlZHVsZWRQcm9jZXNzaW5nID0gbnVsbDtcclxuICAgICAgICB0aGlzLnF1ZXVlRmlsdGVyaW5nKHBhcmFtcy5zdHlsZXNoZWV0cywgcGFyYW1zLm11dGF0aW9ucyk7XHJcbiAgICAgIH1cclxuICAgIH07XHJcblxyXG4gICAgaWYgKHRoaXMuX3NjaGVkdWxlZFByb2Nlc3NpbmcpXHJcbiAgICB7XHJcbiAgICAgIGlmICghc3R5bGVzaGVldHMgJiYgIW11dGF0aW9ucylcclxuICAgICAge1xyXG4gICAgICAgIHRoaXMuX3NjaGVkdWxlZFByb2Nlc3NpbmcgPSB7fTtcclxuICAgICAgfVxyXG4gICAgICBlbHNlIGlmICh0aGlzLl9zY2hlZHVsZWRQcm9jZXNzaW5nLnN0eWxlc2hlZXRzIHx8XHJcbiAgICAgICAgICAgICAgIHRoaXMuX3NjaGVkdWxlZFByb2Nlc3NpbmcubXV0YXRpb25zKVxyXG4gICAgICB7XHJcbiAgICAgICAgaWYgKHN0eWxlc2hlZXRzKVxyXG4gICAgICAgIHtcclxuICAgICAgICAgIGlmICghdGhpcy5fc2NoZWR1bGVkUHJvY2Vzc2luZy5zdHlsZXNoZWV0cylcclxuICAgICAgICAgICAgdGhpcy5fc2NoZWR1bGVkUHJvY2Vzc2luZy5zdHlsZXNoZWV0cyA9IFtdO1xyXG4gICAgICAgICAgdGhpcy5fc2NoZWR1bGVkUHJvY2Vzc2luZy5zdHlsZXNoZWV0cy5wdXNoKC4uLnN0eWxlc2hlZXRzKTtcclxuICAgICAgICB9XHJcbiAgICAgICAgaWYgKG11dGF0aW9ucylcclxuICAgICAgICB7XHJcbiAgICAgICAgICBpZiAoIXRoaXMuX3NjaGVkdWxlZFByb2Nlc3NpbmcubXV0YXRpb25zKVxyXG4gICAgICAgICAgICB0aGlzLl9zY2hlZHVsZWRQcm9jZXNzaW5nLm11dGF0aW9ucyA9IFtdO1xyXG4gICAgICAgICAgdGhpcy5fc2NoZWR1bGVkUHJvY2Vzc2luZy5tdXRhdGlvbnMucHVzaCguLi5tdXRhdGlvbnMpO1xyXG4gICAgICAgIH1cclxuICAgICAgfVxyXG4gICAgfVxyXG4gICAgZWxzZSBpZiAodGhpcy5fZmlsdGVyaW5nSW5Qcm9ncmVzcylcclxuICAgIHtcclxuICAgICAgdGhpcy5fc2NoZWR1bGVkUHJvY2Vzc2luZyA9IHtzdHlsZXNoZWV0cywgbXV0YXRpb25zfTtcclxuICAgIH1cclxuICAgIGVsc2UgaWYgKHBlcmZvcm1hbmNlLm5vdygpIC0gdGhpcy5fbGFzdEludm9jYXRpb24gPFxyXG4gICAgICAgICAgICAgdGhpcy5taW5JbnZvY2F0aW9uSW50ZXJ2YWwpXHJcbiAgICB7XHJcbiAgICAgIHRoaXMuX3NjaGVkdWxlZFByb2Nlc3NpbmcgPSB7c3R5bGVzaGVldHMsIG11dGF0aW9uc307XHJcbiAgICAgIHNldFRpbWVvdXQoXHJcbiAgICAgICAgKCkgPT5cclxuICAgICAgICB7XHJcbiAgICAgICAgICBsZXQgcGFyYW1zID0gT2JqZWN0LmFzc2lnbih7fSwgdGhpcy5fc2NoZWR1bGVkUHJvY2Vzc2luZyk7XHJcbiAgICAgICAgICB0aGlzLl9maWx0ZXJpbmdJblByb2dyZXNzID0gdHJ1ZTtcclxuICAgICAgICAgIHRoaXMuX3NjaGVkdWxlZFByb2Nlc3NpbmcgPSBudWxsO1xyXG4gICAgICAgICAgdGhpcy5fYWRkU2VsZWN0b3JzKHBhcmFtcy5zdHlsZXNoZWV0cywgcGFyYW1zLm11dGF0aW9ucywgY29tcGxldGlvbik7XHJcbiAgICAgICAgfSxcclxuICAgICAgICB0aGlzLm1pbkludm9jYXRpb25JbnRlcnZhbCAtIChwZXJmb3JtYW5jZS5ub3coKSAtIHRoaXMuX2xhc3RJbnZvY2F0aW9uKVxyXG4gICAgICApO1xyXG4gICAgfVxyXG4gICAgZWxzZSBpZiAodGhpcy5kb2N1bWVudC5yZWFkeVN0YXRlID09IFwibG9hZGluZ1wiKVxyXG4gICAge1xyXG4gICAgICB0aGlzLl9zY2hlZHVsZWRQcm9jZXNzaW5nID0ge3N0eWxlc2hlZXRzLCBtdXRhdGlvbnN9O1xyXG4gICAgICBsZXQgaGFuZGxlciA9ICgpID0+XHJcbiAgICAgIHtcclxuICAgICAgICB0aGlzLmRvY3VtZW50LnJlbW92ZUV2ZW50TGlzdGVuZXIoXCJET01Db250ZW50TG9hZGVkXCIsIGhhbmRsZXIpO1xyXG4gICAgICAgIGxldCBwYXJhbXMgPSBPYmplY3QuYXNzaWduKHt9LCB0aGlzLl9zY2hlZHVsZWRQcm9jZXNzaW5nKTtcclxuICAgICAgICB0aGlzLl9maWx0ZXJpbmdJblByb2dyZXNzID0gdHJ1ZTtcclxuICAgICAgICB0aGlzLl9zY2hlZHVsZWRQcm9jZXNzaW5nID0gbnVsbDtcclxuICAgICAgICB0aGlzLl9hZGRTZWxlY3RvcnMocGFyYW1zLnN0eWxlc2hlZXRzLCBwYXJhbXMubXV0YXRpb25zLCBjb21wbGV0aW9uKTtcclxuICAgICAgfTtcclxuICAgICAgdGhpcy5kb2N1bWVudC5hZGRFdmVudExpc3RlbmVyKFwiRE9NQ29udGVudExvYWRlZFwiLCBoYW5kbGVyKTtcclxuICAgIH1cclxuICAgIGVsc2VcclxuICAgIHtcclxuICAgICAgdGhpcy5fZmlsdGVyaW5nSW5Qcm9ncmVzcyA9IHRydWU7XHJcbiAgICAgIHRoaXMuX2FkZFNlbGVjdG9ycyhzdHlsZXNoZWV0cywgbXV0YXRpb25zLCBjb21wbGV0aW9uKTtcclxuICAgIH1cclxuICB9XHJcblxyXG4gIG9uTG9hZChldmVudClcclxuICB7XHJcbiAgICBsZXQgc3R5bGVzaGVldCA9IGV2ZW50LnRhcmdldC5zaGVldDtcclxuICAgIGlmIChzdHlsZXNoZWV0KVxyXG4gICAgICB0aGlzLnF1ZXVlRmlsdGVyaW5nKFtzdHlsZXNoZWV0XSk7XHJcbiAgfVxyXG5cclxuICBvYnNlcnZlKG11dGF0aW9ucylcclxuICB7XHJcbiAgICBpZiAodGVzdEluZm8pXHJcbiAgICB7XHJcbiAgICAgIC8vIEluIHRlc3QgbW9kZSwgZmlsdGVyIG91dCBhbnkgbXV0YXRpb25zIGxpa2VseSBkb25lIGJ5IHVzXHJcbiAgICAgIC8vIChpLmUuIHN0eWxlPVwiZGlzcGxheTogbm9uZSAhaW1wb3J0YW50XCIpLiBUaGlzIG1ha2VzIGl0IGVhc2llciB0b1xyXG4gICAgICAvLyBvYnNlcnZlIGhvdyB0aGUgY29kZSByZXNwb25kcyB0byBET00gbXV0YXRpb25zLlxyXG4gICAgICBtdXRhdGlvbnMgPSBtdXRhdGlvbnMuZmlsdGVyKFxyXG4gICAgICAgICh7dHlwZSwgYXR0cmlidXRlTmFtZSwgdGFyZ2V0OiB7c3R5bGU6IG5ld1ZhbHVlfSwgb2xkVmFsdWV9KSA9PlxyXG4gICAgICAgICAgISh0eXBlID09IFwiYXR0cmlidXRlc1wiICYmIGF0dHJpYnV0ZU5hbWUgPT0gXCJzdHlsZVwiICYmXHJcbiAgICAgICAgICAgIG5ld1ZhbHVlLmRpc3BsYXkgPT0gXCJub25lXCIgJiZcclxuICAgICAgICAgICAgdG9DU1NTdHlsZURlY2xhcmF0aW9uKG9sZFZhbHVlKS5kaXNwbGF5ICE9IFwibm9uZVwiKVxyXG4gICAgICApO1xyXG5cclxuICAgICAgaWYgKG11dGF0aW9ucy5sZW5ndGggPT0gMClcclxuICAgICAgICByZXR1cm47XHJcbiAgICB9XHJcblxyXG4gICAgdGhpcy5xdWV1ZUZpbHRlcmluZyhudWxsLCBtdXRhdGlvbnMpO1xyXG4gIH1cclxuXHJcbiAgYXBwbHkocGF0dGVybnMpXHJcbiAge1xyXG4gICAgdGhpcy5wYXR0ZXJucyA9IFtdO1xyXG4gICAgZm9yIChsZXQgcGF0dGVybiBvZiBwYXR0ZXJucylcclxuICAgIHtcclxuICAgICAgbGV0IHNlbGVjdG9ycyA9IHRoaXMucGFyc2VTZWxlY3RvcihwYXR0ZXJuLnNlbGVjdG9yKTtcclxuICAgICAgaWYgKHNlbGVjdG9ycyAhPSBudWxsICYmIHNlbGVjdG9ycy5sZW5ndGggPiAwKVxyXG4gICAgICAgIHRoaXMucGF0dGVybnMucHVzaChuZXcgUGF0dGVybihzZWxlY3RvcnMsIHBhdHRlcm4udGV4dCkpO1xyXG4gICAgfVxyXG5cclxuICAgIGlmICh0aGlzLnBhdHRlcm5zLmxlbmd0aCA+IDApXHJcbiAgICB7XHJcbiAgICAgIHRoaXMucXVldWVGaWx0ZXJpbmcoKTtcclxuICAgICAgbGV0IGF0dHJpYnV0ZXMgPSBzaG91bGRPYnNlcnZlQXR0cmlidXRlcyh0aGlzLnBhdHRlcm5zKTtcclxuICAgICAgdGhpcy5vYnNlcnZlci5vYnNlcnZlKFxyXG4gICAgICAgIHRoaXMuZG9jdW1lbnQsXHJcbiAgICAgICAge1xyXG4gICAgICAgICAgY2hpbGRMaXN0OiB0cnVlLFxyXG4gICAgICAgICAgYXR0cmlidXRlcyxcclxuICAgICAgICAgIGF0dHJpYnV0ZU9sZFZhbHVlOiBhdHRyaWJ1dGVzICYmICEhdGVzdEluZm8sXHJcbiAgICAgICAgICBjaGFyYWN0ZXJEYXRhOiBzaG91bGRPYnNlcnZlQ2hhcmFjdGVyRGF0YSh0aGlzLnBhdHRlcm5zKSxcclxuICAgICAgICAgIHN1YnRyZWU6IHRydWVcclxuICAgICAgICB9XHJcbiAgICAgICk7XHJcbiAgICAgIHRoaXMuZG9jdW1lbnQuYWRkRXZlbnRMaXN0ZW5lcihcImxvYWRcIiwgdGhpcy5vbkxvYWQuYmluZCh0aGlzKSwgdHJ1ZSk7XHJcbiAgICB9XHJcbiAgfVxyXG59O1xyXG4iLCIvKlxyXG4gKiBUaGlzIGZpbGUgaXMgcGFydCBvZiBBZGJsb2NrIFBsdXMgPGh0dHBzOi8vYWRibG9ja3BsdXMub3JnLz4sXHJcbiAqIENvcHlyaWdodCAoQykgMjAwNi1wcmVzZW50IGV5ZW8gR21iSFxyXG4gKlxyXG4gKiBBZGJsb2NrIFBsdXMgaXMgZnJlZSBzb2Z0d2FyZTogeW91IGNhbiByZWRpc3RyaWJ1dGUgaXQgYW5kL29yIG1vZGlmeVxyXG4gKiBpdCB1bmRlciB0aGUgdGVybXMgb2YgdGhlIEdOVSBHZW5lcmFsIFB1YmxpYyBMaWNlbnNlIHZlcnNpb24gMyBhc1xyXG4gKiBwdWJsaXNoZWQgYnkgdGhlIEZyZWUgU29mdHdhcmUgRm91bmRhdGlvbi5cclxuICpcclxuICogQWRibG9jayBQbHVzIGlzIGRpc3RyaWJ1dGVkIGluIHRoZSBob3BlIHRoYXQgaXQgd2lsbCBiZSB1c2VmdWwsXHJcbiAqIGJ1dCBXSVRIT1VUIEFOWSBXQVJSQU5UWTsgd2l0aG91dCBldmVuIHRoZSBpbXBsaWVkIHdhcnJhbnR5IG9mXHJcbiAqIE1FUkNIQU5UQUJJTElUWSBvciBGSVRORVNTIEZPUiBBIFBBUlRJQ1VMQVIgUFVSUE9TRS4gIFNlZSB0aGVcclxuICogR05VIEdlbmVyYWwgUHVibGljIExpY2Vuc2UgZm9yIG1vcmUgZGV0YWlscy5cclxuICpcclxuICogWW91IHNob3VsZCBoYXZlIHJlY2VpdmVkIGEgY29weSBvZiB0aGUgR05VIEdlbmVyYWwgUHVibGljIExpY2Vuc2VcclxuICogYWxvbmcgd2l0aCBBZGJsb2NrIFBsdXMuICBJZiBub3QsIHNlZSA8aHR0cDovL3d3dy5nbnUub3JnL2xpY2Vuc2VzLz4uXHJcbiAqL1xyXG5cclxuaW1wb3J0IHtjb2xsYXBzZUVsZW1lbnQsIGNvbnRlbnRGaWx0ZXJpbmcsXHJcbiAgICAgICAgZ2V0VVJMRnJvbUVsZW1lbnR9IGZyb20gXCIuL2luY2x1ZGUucHJlbG9hZC5qc1wiO1xyXG5cclxuLy8gVGhlIHBhZ2UgSUQgZm9yIHRoZSBwb3B1cCBmaWx0ZXIgc2VsZWN0aW9uIGRpYWxvZyAodG9wIGZyYW1lIG9ubHkpLlxyXG5sZXQgYmxvY2tlbGVtZW50UG9wdXBJZCA9IG51bGw7XHJcblxyXG4vLyBFbGVtZW50IHBpY2tpbmcgc3RhdGUgKHRvcCBmcmFtZSBvbmx5KS5cclxubGV0IGN1cnJlbnRseVBpY2tpbmdFbGVtZW50ID0gZmFsc2U7XHJcbmxldCBsYXN0TW91c2VPdmVyRXZlbnQgPSBudWxsO1xyXG5cclxuLy8gRHVyaW5nIGVsZW1lbnQgcGlja2luZyB0aGlzIGlzIHRoZSBjdXJyZW50bHkgaGlnaGxpZ2h0ZWQgZWxlbWVudC4gV2hlblxyXG4vLyBlbGVtZW50IGhhcyBiZWVuIHBpY2tlZCB0aGlzIGlzIHRoZSBlbGVtZW50IHRoYXQgaXMgZHVlIHRvIGJlIGJsb2NrZWQuXHJcbmxldCBjdXJyZW50RWxlbWVudCA9IG51bGw7XHJcblxyXG4vLyBIaWdobGlnaHRpbmcgc3RhdGUsIHVzZWQgYnkgdGhlIHRvcCBmcmFtZSBkdXJpbmcgZWxlbWVudCBwaWNraW5nIGFuZCBhbGxcclxuLy8gZnJhbWVzIHdoZW4gdGhlIGNob3NlbiBlbGVtZW50IGlzIGhpZ2hsaWdodGVkIHJlZC5cclxubGV0IGhpZ2hsaWdodGVkRWxlbWVudHNTZWxlY3RvciA9IG51bGw7XHJcbmxldCBoaWdobGlnaHRlZEVsZW1lbnRzSW50ZXJ2YWwgPSBudWxsO1xyXG5cclxuLy8gTGFzdCByaWdodCBjbGljayBzdGF0ZSBzdG9yZWQgZm9yIGVsZW1lbnQgYmxvY2tpbmcgdmlhIHRoZSBjb250ZXh0IG1lbnUuXHJcbmxldCBsYXN0UmlnaHRDbGlja0V2ZW50ID0gbnVsbDtcclxubGV0IGxhc3RSaWdodENsaWNrRXZlbnRJc01vc3RSZWNlbnQgPSBmYWxzZTtcclxuXHJcblxyXG4vKiBVdGlsaXRpZXMgKi9cclxuXHJcbmZ1bmN0aW9uIGdldEZpbHRlcnNGb3JFbGVtZW50KGVsZW1lbnQpXHJcbntcclxuICBsZXQgc3JjID0gZWxlbWVudC5nZXRBdHRyaWJ1dGUoXCJzcmNcIik7XHJcbiAgcmV0dXJuIGJyb3dzZXIucnVudGltZS5zZW5kTWVzc2FnZSh7XHJcbiAgICB0eXBlOiBcImNvbXBvc2VyLmdldEZpbHRlcnNcIixcclxuICAgIHRhZ05hbWU6IGVsZW1lbnQubG9jYWxOYW1lLFxyXG4gICAgaWQ6IGVsZW1lbnQuaWQsXHJcbiAgICBzcmM6IHNyYyAmJiBzcmMubGVuZ3RoIDw9IDEwMDAgPyBzcmMgOiBudWxsLFxyXG4gICAgc3R5bGU6IGVsZW1lbnQuZ2V0QXR0cmlidXRlKFwic3R5bGVcIiksXHJcbiAgICBjbGFzc2VzOiBBcnJheS5wcm90b3R5cGUuc2xpY2UuY2FsbChlbGVtZW50LmNsYXNzTGlzdCksXHJcbiAgICB1cmw6IGdldFVSTEZyb21FbGVtZW50KGVsZW1lbnQpXHJcbiAgfSk7XHJcbn1cclxuXHJcbmFzeW5jIGZ1bmN0aW9uIGdldEJsb2NrYWJsZUVsZW1lbnRPckFuY2VzdG9yKGVsZW1lbnQpXHJcbntcclxuICAvLyBXZSBhc3N1bWUgdGhhdCB0aGUgdXNlciBkb2Vzbid0IHdhbnQgdG8gYmxvY2sgdGhlIHdob2xlIHBhZ2UuXHJcbiAgLy8gU28gd2UgbmV2ZXIgY29uc2lkZXIgdGhlIDxodG1sPiBvciA8Ym9keT4gZWxlbWVudC5cclxuICB3aGlsZSAoZWxlbWVudCAmJiBlbGVtZW50ICE9IGRvY3VtZW50LmRvY3VtZW50RWxlbWVudCAmJlxyXG4gICAgICAgICBlbGVtZW50ICE9IGRvY3VtZW50LmJvZHkpXHJcbiAge1xyXG4gICAgLy8gV2UgY2FuJ3QgaGFuZGxlIG5vbi1IVE1MIChsaWtlIFNWRykgZWxlbWVudHMsIGFzIHdlbGwgYXNcclxuICAgIC8vIDxhcmVhPiBlbGVtZW50cyAoc2VlIGJlbG93KS4gU28gZmFsbCBiYWNrIHRvIHRoZSBwYXJlbnQgZWxlbWVudC5cclxuICAgIGlmICghKGVsZW1lbnQgaW5zdGFuY2VvZiBIVE1MRWxlbWVudCkgfHwgZWxlbWVudC5sb2NhbE5hbWUgPT0gXCJhcmVhXCIpXHJcbiAgICB7XHJcbiAgICAgIGVsZW1lbnQgPSBlbGVtZW50LnBhcmVudEVsZW1lbnQ7XHJcbiAgICB9XHJcbiAgICAvLyBJZiBpbWFnZSBtYXBzIGFyZSB1c2VkIG1vdXNlIGV2ZW50cyBvY2N1ciBmb3IgdGhlIDxhcmVhPiBlbGVtZW50LlxyXG4gICAgLy8gQnV0IHdlIGhhdmUgdG8gYmxvY2sgdGhlIGltYWdlIGFzc29jaWF0ZWQgd2l0aCB0aGUgPG1hcD4gZWxlbWVudC5cclxuICAgIGVsc2UgaWYgKGVsZW1lbnQubG9jYWxOYW1lID09IFwibWFwXCIpXHJcbiAgICB7XHJcbiAgICAgIGxldCBpbWFnZXMgPSBkb2N1bWVudC5xdWVyeVNlbGVjdG9yQWxsKFwiaW1nW3VzZW1hcF1cIik7XHJcbiAgICAgIGxldCBpbWFnZSA9IG51bGw7XHJcblxyXG4gICAgICBmb3IgKGxldCBjdXJyZW50SW1hZ2Ugb2YgaW1hZ2VzKVxyXG4gICAgICB7XHJcbiAgICAgICAgbGV0IHVzZW1hcCA9IGN1cnJlbnRJbWFnZS5nZXRBdHRyaWJ1dGUoXCJ1c2VtYXBcIik7XHJcbiAgICAgICAgbGV0IGluZGV4ID0gdXNlbWFwLmluZGV4T2YoXCIjXCIpO1xyXG5cclxuICAgICAgICBpZiAoaW5kZXggIT0gLTEgJiYgdXNlbWFwLnN1YnN0cihpbmRleCArIDEpID09IGVsZW1lbnQubmFtZSlcclxuICAgICAgICB7XHJcbiAgICAgICAgICBpbWFnZSA9IGN1cnJlbnRJbWFnZTtcclxuICAgICAgICAgIGJyZWFrO1xyXG4gICAgICAgIH1cclxuICAgICAgfVxyXG5cclxuICAgICAgZWxlbWVudCA9IGltYWdlO1xyXG4gICAgfVxyXG5cclxuICAgIC8vIEZpbmFsbHksIGlmIG5vbmUgb2YgdGhlIGFib3ZlIGlzIHRydWUsIGNoZWNrIHdoZXRoZXIgd2UgY2FuIGdlbmVyYXRlXHJcbiAgICAvLyBhbnkgZmlsdGVycyBmb3IgdGhpcyBlbGVtZW50LiBPdGhlcndpc2UgZmFsbCBiYWNrIHRvIGl0cyBwYXJlbnQgZWxlbWVudC5cclxuICAgIGVsc2VcclxuICAgIHtcclxuICAgICAgbGV0IHtmaWx0ZXJzfSA9IGF3YWl0IGdldEZpbHRlcnNGb3JFbGVtZW50KGVsZW1lbnQpO1xyXG4gICAgICBpZiAoZmlsdGVycy5sZW5ndGggPiAwKVxyXG4gICAgICAgIHJldHVybiBlbGVtZW50O1xyXG4gICAgICByZXR1cm4gZ2V0QmxvY2thYmxlRWxlbWVudE9yQW5jZXN0b3IoZWxlbWVudC5wYXJlbnRFbGVtZW50KTtcclxuICAgIH1cclxuICB9XHJcblxyXG4gIC8vIFdlIHJlYWNoZWQgdGhlIGRvY3VtZW50IHJvb3Qgd2l0aG91dCBmaW5kaW5nIGEgYmxvY2thYmxlIGVsZW1lbnQuXHJcbiAgcmV0dXJuIG51bGw7XHJcbn1cclxuXHJcblxyXG4vKiBFbGVtZW50IGhpZ2hsaWdodGluZyAqL1xyXG5cclxuLy8gQWRkcyBhbiBvdmVybGF5IHRvIGFuIGVsZW1lbnQgaW4gb3JkZXIgdG8gaGlnaGxpZ2h0IGl0LlxyXG5mdW5jdGlvbiBhZGRFbGVtZW50T3ZlcmxheShlbGVtZW50KVxyXG57XHJcbiAgbGV0IHBvc2l0aW9uID0gXCJhYnNvbHV0ZVwiO1xyXG4gIGxldCBvZmZzZXRYID0gd2luZG93LnNjcm9sbFg7XHJcbiAgbGV0IG9mZnNldFkgPSB3aW5kb3cuc2Nyb2xsWTtcclxuXHJcbiAgZm9yIChsZXQgZSA9IGVsZW1lbnQ7IGU7IGUgPSBlLnBhcmVudEVsZW1lbnQpXHJcbiAge1xyXG4gICAgbGV0IHN0eWxlID0gZ2V0Q29tcHV0ZWRTdHlsZShlKTtcclxuXHJcbiAgICAvLyBJZiB0aGUgZWxlbWVudCBpc24ndCByZW5kZXJlZCAoc2luY2UgaXRzIG9yIG9uZSBvZiBpdHMgYW5jZXN0b3Inc1xyXG4gICAgLy8gXCJkaXNwbGF5XCIgcHJvcGVydHkgaXMgXCJub25lXCIpLCB0aGUgb3ZlcmxheSB3b3VsZG4ndCBtYXRjaCB0aGUgZWxlbWVudC5cclxuICAgIGlmIChzdHlsZS5kaXNwbGF5ID09IFwibm9uZVwiKVxyXG4gICAgICByZXR1cm4gbnVsbDtcclxuXHJcbiAgICAvLyBJZiB0aGUgZWxlbWVudCBvciBvbmUgb2YgaXRzIGFuY2VzdG9ycyB1c2VzIGZpeGVkIHBvc3Rpb25pbmcsIHRoZSBvdmVybGF5XHJcbiAgICAvLyBtdXN0IHRvby4gT3RoZXJ3aXNlIGl0cyBwb3NpdGlvbiBtaWdodCBub3QgbWF0Y2ggdGhlIGVsZW1lbnQncy5cclxuICAgIGlmIChzdHlsZS5wb3NpdGlvbiA9PSBcImZpeGVkXCIpXHJcbiAgICB7XHJcbiAgICAgIHBvc2l0aW9uID0gXCJmaXhlZFwiO1xyXG4gICAgICBvZmZzZXRYID0gb2Zmc2V0WSA9IDA7XHJcbiAgICB9XHJcbiAgfVxyXG5cclxuICBsZXQgb3ZlcmxheSA9IGRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoXCJkaXZcIik7XHJcbiAgb3ZlcmxheS5wcmlzb25lciA9IGVsZW1lbnQ7XHJcbiAgb3ZlcmxheS5jbGFzc05hbWUgPSBcIl9fYWRibG9ja3BsdXNfX292ZXJsYXlcIjtcclxuICBvdmVybGF5LnNldEF0dHJpYnV0ZShcInN0eWxlXCIsXHJcbiAgICAgICAgICAgICAgICAgICAgICAgXCJvcGFjaXR5OjAuNDsgZGlzcGxheTppbmxpbmUtYmxvY2sgIWltcG9ydGFudDsgXCIgK1xyXG4gICAgICAgICAgICAgICAgICAgICAgIFwib3ZlcmZsb3c6aGlkZGVuOyBib3gtc2l6aW5nOmJvcmRlci1ib3g7XCIpO1xyXG4gIGxldCByZWN0ID0gZWxlbWVudC5nZXRCb3VuZGluZ0NsaWVudFJlY3QoKTtcclxuICBvdmVybGF5LnN0eWxlLndpZHRoID0gcmVjdC53aWR0aCArIFwicHhcIjtcclxuICBvdmVybGF5LnN0eWxlLmhlaWdodCA9IHJlY3QuaGVpZ2h0ICsgXCJweFwiO1xyXG4gIG92ZXJsYXkuc3R5bGUubGVmdCA9IChyZWN0LmxlZnQgKyBvZmZzZXRYKSArIFwicHhcIjtcclxuICBvdmVybGF5LnN0eWxlLnRvcCA9IChyZWN0LnRvcCArIG9mZnNldFkpICsgXCJweFwiO1xyXG4gIG92ZXJsYXkuc3R5bGUucG9zaXRpb24gPSBwb3NpdGlvbjtcclxuICBvdmVybGF5LnN0eWxlLnpJbmRleCA9IDB4N0ZGRkZGRkU7XHJcblxyXG4gIGRvY3VtZW50LmRvY3VtZW50RWxlbWVudC5hcHBlbmRDaGlsZChvdmVybGF5KTtcclxuICByZXR1cm4gb3ZlcmxheTtcclxufVxyXG5cclxuZnVuY3Rpb24gaGlnaGxpZ2h0RWxlbWVudChlbGVtZW50LCBib3JkZXIsIGJhY2tncm91bmRDb2xvcilcclxue1xyXG4gIHVuaGlnaGxpZ2h0RWxlbWVudChlbGVtZW50KTtcclxuXHJcbiAgbGV0IGhpZ2hsaWdodFdpdGhPdmVybGF5ID0gKCkgPT5cclxuICB7XHJcbiAgICBsZXQgb3ZlcmxheSA9IGFkZEVsZW1lbnRPdmVybGF5KGVsZW1lbnQpO1xyXG5cclxuICAgIC8vIElmIHRoZSBlbGVtZW50IGlzbid0IGRpc3BsYXllZCBubyBvdmVybGF5IHdpbGwgYmUgYWRkZWQuXHJcbiAgICAvLyBNb3Jlb3Zlciwgd2UgZG9uJ3QgbmVlZCB0byBoaWdobGlnaHQgYW55dGhpbmcgdGhlbi5cclxuICAgIGlmICghb3ZlcmxheSlcclxuICAgICAgcmV0dXJuO1xyXG5cclxuICAgIGhpZ2hsaWdodEVsZW1lbnQob3ZlcmxheSwgYm9yZGVyLCBiYWNrZ3JvdW5kQ29sb3IpO1xyXG4gICAgb3ZlcmxheS5zdHlsZS5wb2ludGVyRXZlbnRzID0gXCJub25lXCI7XHJcblxyXG4gICAgZWxlbWVudC5fdW5oaWdobGlnaHQgPSAoKSA9PlxyXG4gICAge1xyXG4gICAgICBvdmVybGF5LnBhcmVudE5vZGUucmVtb3ZlQ2hpbGQob3ZlcmxheSk7XHJcbiAgICB9O1xyXG4gIH07XHJcblxyXG4gIGxldCBoaWdobGlnaHRXaXRoU3R5bGVBdHRyaWJ1dGUgPSAoKSA9PlxyXG4gIHtcclxuICAgIGxldCBvcmlnaW5hbEJvcmRlciA9IGVsZW1lbnQuc3R5bGUuZ2V0UHJvcGVydHlWYWx1ZShcImJvcmRlclwiKTtcclxuICAgIGxldCBvcmlnaW5hbEJvcmRlclByaW9yaXR5ID1cclxuICAgICAgZWxlbWVudC5zdHlsZS5nZXRQcm9wZXJ0eVByaW9yaXR5KFwiYm94LXNoYWRvd1wiKTtcclxuICAgIGxldCBvcmlnaW5hbEJhY2tncm91bmRDb2xvciA9XHJcbiAgICAgIGVsZW1lbnQuc3R5bGUuZ2V0UHJvcGVydHlWYWx1ZShcImJhY2tncm91bmQtY29sb3JcIik7XHJcbiAgICBsZXQgb3JpZ2luYWxCYWNrZ3JvdW5kQ29sb3JQcmlvcml0eSA9XHJcbiAgICAgIGVsZW1lbnQuc3R5bGUuZ2V0UHJvcGVydHlQcmlvcml0eShcImJhY2tncm91bmQtY29sb3JcIik7XHJcblxyXG4gICAgZWxlbWVudC5zdHlsZS5zZXRQcm9wZXJ0eShcImJvcmRlclwiLCBgMnB4IHNvbGlkICR7Ym9yZGVyfWAsIFwiaW1wb3J0YW50XCIpO1xyXG4gICAgZWxlbWVudC5zdHlsZS5zZXRQcm9wZXJ0eShcImJhY2tncm91bmQtY29sb3JcIiwgYmFja2dyb3VuZENvbG9yLCBcImltcG9ydGFudFwiKTtcclxuXHJcbiAgICBlbGVtZW50Ll91bmhpZ2hsaWdodCA9ICgpID0+XHJcbiAgICB7XHJcbiAgICAgIGVsZW1lbnQuc3R5bGUucmVtb3ZlUHJvcGVydHkoXCJib3gtc2hhZG93XCIpO1xyXG4gICAgICBlbGVtZW50LnN0eWxlLnNldFByb3BlcnR5KFxyXG4gICAgICAgIFwiYm9yZGVyXCIsXHJcbiAgICAgICAgb3JpZ2luYWxCb3JkZXIsXHJcbiAgICAgICAgb3JpZ2luYWxCb3JkZXJQcmlvcml0eVxyXG4gICAgICApO1xyXG5cclxuICAgICAgZWxlbWVudC5zdHlsZS5yZW1vdmVQcm9wZXJ0eShcImJhY2tncm91bmQtY29sb3JcIik7XHJcbiAgICAgIGVsZW1lbnQuc3R5bGUuc2V0UHJvcGVydHkoXHJcbiAgICAgICAgXCJiYWNrZ3JvdW5kLWNvbG9yXCIsXHJcbiAgICAgICAgb3JpZ2luYWxCYWNrZ3JvdW5kQ29sb3IsXHJcbiAgICAgICAgb3JpZ2luYWxCYWNrZ3JvdW5kQ29sb3JQcmlvcml0eVxyXG4gICAgICApO1xyXG4gICAgfTtcclxuICB9O1xyXG5cclxuICAvLyBJZiB0aGlzIGVsZW1lbnQgaXMgYW4gb3ZlcmxheSB0aGF0IHdlJ3ZlIGNyZWF0ZWQgcHJldmlvdXNseSB0aGVuIHdlIG5lZWRcclxuICAvLyB0byBnaXZlIGl0IGEgYmFja2dyb3VuZCBjb2xvdXIuIE90aGVyd2lzZSB3ZSBuZWVkIHRvIGNyZWF0ZSBhbiBvdmVybGF5XHJcbiAgLy8gYW5kIHRoZW4gcmVjdXJzZSBpbiBvcmRlciB0byBzZXQgdGhlIG92ZXJsYXkncyBiYWNrZ3JvdW5kIGNvbG91ci5cclxuICBpZiAoXCJwcmlzb25lclwiIGluIGVsZW1lbnQpXHJcbiAgICBoaWdobGlnaHRXaXRoU3R5bGVBdHRyaWJ1dGUoKTtcclxuICBlbHNlXHJcbiAgICBoaWdobGlnaHRXaXRoT3ZlcmxheSgpO1xyXG59XHJcblxyXG5mdW5jdGlvbiB1bmhpZ2hsaWdodEVsZW1lbnQoZWxlbWVudClcclxue1xyXG4gIGlmIChlbGVtZW50ICYmIFwiX3VuaGlnaGxpZ2h0XCIgaW4gZWxlbWVudClcclxuICB7XHJcbiAgICBlbGVtZW50Ll91bmhpZ2hsaWdodCgpO1xyXG4gICAgZGVsZXRlIGVsZW1lbnQuX3VuaGlnaGxpZ2h0O1xyXG4gIH1cclxufVxyXG5cclxuLy8gSGlnaGxpZ2h0IGVsZW1lbnRzIG1hdGNoaW5nIHRoZSBzZWxlY3RvciBzdHJpbmcgcmVkLlxyXG4vLyAoQWxsIGVsZW1lbnRzIHRoYXQgd291bGQgYmUgYmxvY2tlZCBieSB0aGUgcHJvcG9zZWQgZmlsdGVycy4pXHJcbmZ1bmN0aW9uIGhpZ2hsaWdodEVsZW1lbnRzKHNlbGVjdG9yU3RyaW5nKVxyXG57XHJcbiAgdW5oaWdobGlnaHRFbGVtZW50cygpO1xyXG5cclxuICBsZXQgZWxlbWVudHMgPSBBcnJheS5wcm90b3R5cGUuc2xpY2UuY2FsbChcclxuICAgIGRvY3VtZW50LnF1ZXJ5U2VsZWN0b3JBbGwoc2VsZWN0b3JTdHJpbmcpXHJcbiAgKTtcclxuICBoaWdobGlnaHRlZEVsZW1lbnRzU2VsZWN0b3IgPSBzZWxlY3RvclN0cmluZztcclxuXHJcbiAgLy8gSGlnaGxpZ2h0IGVsZW1lbnRzIHByb2dyZXNzaXZlbHkuIE90aGVyd2lzZSB0aGUgcGFnZSBmcmVlemVzXHJcbiAgLy8gd2hlbiBhIGxvdCBvZiBlbGVtZW50cyBnZXQgaGlnaGxpZ2h0ZWQgYXQgdGhlIHNhbWUgdGltZS5cclxuICBoaWdobGlnaHRlZEVsZW1lbnRzSW50ZXJ2YWwgPSBzZXRJbnRlcnZhbCgoKSA9PlxyXG4gIHtcclxuICAgIGlmIChlbGVtZW50cy5sZW5ndGggPiAwKVxyXG4gICAge1xyXG4gICAgICBsZXQgZWxlbWVudCA9IGVsZW1lbnRzLnNoaWZ0KCk7XHJcbiAgICAgIGlmIChlbGVtZW50ICE9IGN1cnJlbnRFbGVtZW50KVxyXG4gICAgICAgIGhpZ2hsaWdodEVsZW1lbnQoZWxlbWVudCwgXCIjQ0EwMDAwXCIsIFwiI0NBMDAwMFwiKTtcclxuICAgIH1cclxuICAgIGVsc2VcclxuICAgIHtcclxuICAgICAgY2xlYXJJbnRlcnZhbChoaWdobGlnaHRlZEVsZW1lbnRzSW50ZXJ2YWwpO1xyXG4gICAgICBoaWdobGlnaHRlZEVsZW1lbnRzSW50ZXJ2YWwgPSBudWxsO1xyXG4gICAgfVxyXG4gIH0sIDApO1xyXG59XHJcblxyXG4vLyBVbmhpZ2hsaWdodCB0aGUgZWxlbWVudHMgdGhhdCB3ZXJlIGhpZ2hsaWdodGVkIGJ5IHNlbGVjdG9yIHN0cmluZyBwcmV2aW91c2x5LlxyXG5mdW5jdGlvbiB1bmhpZ2hsaWdodEVsZW1lbnRzKClcclxue1xyXG4gIGlmIChoaWdobGlnaHRlZEVsZW1lbnRzSW50ZXJ2YWwpXHJcbiAge1xyXG4gICAgY2xlYXJJbnRlcnZhbChoaWdobGlnaHRlZEVsZW1lbnRzSW50ZXJ2YWwpO1xyXG4gICAgaGlnaGxpZ2h0ZWRFbGVtZW50c0ludGVydmFsID0gbnVsbDtcclxuICB9XHJcblxyXG4gIGlmIChoaWdobGlnaHRlZEVsZW1lbnRzU2VsZWN0b3IpXHJcbiAge1xyXG4gICAgQXJyYXkucHJvdG90eXBlLmZvckVhY2guY2FsbChcclxuICAgICAgZG9jdW1lbnQucXVlcnlTZWxlY3RvckFsbChoaWdobGlnaHRlZEVsZW1lbnRzU2VsZWN0b3IpLFxyXG4gICAgICB1bmhpZ2hsaWdodEVsZW1lbnRcclxuICAgICk7XHJcblxyXG4gICAgaGlnaGxpZ2h0ZWRFbGVtZW50c1NlbGVjdG9yID0gbnVsbDtcclxuICB9XHJcbn1cclxuXHJcblxyXG4vKiBJbnB1dCBldmVudCBoYW5kbGVycyAqL1xyXG5cclxuZnVuY3Rpb24gc3RvcEV2ZW50UHJvcGFnYXRpb24oZXZlbnQpXHJcbntcclxuICBldmVudC5zdG9wUHJvcGFnYXRpb24oKTtcclxufVxyXG5cclxuLy8gSG92ZXJpbmcgb3ZlciBhbiBlbGVtZW50IHNvIGhpZ2hsaWdodCBpdC5cclxuYXN5bmMgZnVuY3Rpb24gbW91c2VPdmVyKGV2ZW50KVxyXG57XHJcbiAgbGFzdE1vdXNlT3ZlckV2ZW50ID0gZXZlbnQ7XHJcblxyXG4gIGxldCBlbGVtZW50ID0gYXdhaXQgZ2V0QmxvY2thYmxlRWxlbWVudE9yQW5jZXN0b3IoZXZlbnQudGFyZ2V0KTtcclxuICBpZiAoZXZlbnQgPT0gbGFzdE1vdXNlT3ZlckV2ZW50KVxyXG4gIHtcclxuICAgIGxhc3RNb3VzZU92ZXJFdmVudCA9IG51bGw7XHJcblxyXG4gICAgaWYgKGN1cnJlbnRseVBpY2tpbmdFbGVtZW50KVxyXG4gICAge1xyXG4gICAgICBpZiAoY3VycmVudEVsZW1lbnQpXHJcbiAgICAgICAgdW5oaWdobGlnaHRFbGVtZW50KGN1cnJlbnRFbGVtZW50KTtcclxuXHJcbiAgICAgIGlmIChlbGVtZW50KVxyXG4gICAgICAgIGhpZ2hsaWdodEVsZW1lbnQoZWxlbWVudCwgXCIjQ0EwMDAwXCIsIFwiI0NBMDAwMFwiKTtcclxuXHJcbiAgICAgIGN1cnJlbnRFbGVtZW50ID0gZWxlbWVudDtcclxuICAgIH1cclxuICB9XHJcblxyXG4gIGV2ZW50LnN0b3BQcm9wYWdhdGlvbigpO1xyXG59XHJcblxyXG4vLyBObyBsb25nZXIgaG92ZXJpbmcgb3ZlciB0aGlzIGVsZW1lbnQgc28gdW5oaWdobGlnaHQgaXQuXHJcbmZ1bmN0aW9uIG1vdXNlT3V0KGV2ZW50KVxyXG57XHJcbiAgaWYgKCFjdXJyZW50bHlQaWNraW5nRWxlbWVudCB8fCBjdXJyZW50RWxlbWVudCAhPSBldmVudC50YXJnZXQpXHJcbiAgICByZXR1cm47XHJcblxyXG4gIHVuaGlnaGxpZ2h0RWxlbWVudChjdXJyZW50RWxlbWVudCk7XHJcbiAgZXZlbnQuc3RvcFByb3BhZ2F0aW9uKCk7XHJcbn1cclxuXHJcbi8vIEtleSBldmVudHMgLSBSZXR1cm4gc2VsZWN0cyBjdXJyZW50bHkgaG92ZXJlZC1vdmVyIGVsZW1lbnQsIGVzY2FwZSBhYm9ydHMuXHJcbmZ1bmN0aW9uIGtleURvd24oZXZlbnQpXHJcbntcclxuICBpZiAoIWV2ZW50LmN0cmxLZXkgJiYgIWV2ZW50LmFsdEtleSAmJiAhZXZlbnQuc2hpZnRLZXkpXHJcbiAge1xyXG4gICAgaWYgKGV2ZW50LmtleUNvZGUgPT0gMTMpIC8vIFJldHVyblxyXG4gICAgICBlbGVtZW50UGlja2VkKGV2ZW50KTtcclxuICAgIGVsc2UgaWYgKGV2ZW50LmtleUNvZGUgPT0gMjcpIC8vIEVzY2FwZVxyXG4gICAgICBkZWFjdGl2YXRlQmxvY2tFbGVtZW50KCk7XHJcbiAgfVxyXG59XHJcblxyXG5cclxuLyogRWxlbWVudCBzZWxlY3Rpb24gKi9cclxuXHJcbi8vIFN0YXJ0IGhpZ2hsaWdodGluZyBlbGVtZW50cyB5ZWxsb3cgYXMgdGhlIG1vdXNlIG1vdmVzIG92ZXIgdGhlbSwgd2hlbiBvbmUgaXNcclxuLy8gY2hvc2VuIGxhdW5jaCB0aGUgcG9wdXAgZGlhbG9nIGZvciB0aGUgdXNlciB0byBjb25maXJtIHRoZSBnZW5lcmF0ZWQgZmlsdGVycy5cclxuZnVuY3Rpb24gc3RhcnRQaWNraW5nRWxlbWVudCgpXHJcbntcclxuICBjdXJyZW50bHlQaWNraW5nRWxlbWVudCA9IHRydWU7XHJcblxyXG4gIC8vIEFkZCAoY3VycmVudGx5IGludmlzaWJsZSkgb3ZlcmxheXMgZm9yIGJsb2NrYWJsZSBlbGVtZW50cyB0aGF0IGRvbid0IGVtaXRcclxuICAvLyBtb3VzZSBldmVudHMsIHNvIHRoYXQgdGhleSBjYW4gc3RpbGwgYmUgc2VsZWN0ZWQuXHJcbiAgQXJyYXkucHJvdG90eXBlLmZvckVhY2guY2FsbChcclxuICAgIGRvY3VtZW50LnF1ZXJ5U2VsZWN0b3JBbGwoXCJvYmplY3QsZW1iZWQsaWZyYW1lLGZyYW1lXCIpLFxyXG4gICAgYXN5bmMgZWxlbWVudCA9PlxyXG4gICAge1xyXG4gICAgICBsZXQge2ZpbHRlcnN9ID0gYXdhaXQgZ2V0RmlsdGVyc0ZvckVsZW1lbnQoZWxlbWVudCk7XHJcbiAgICAgIGlmIChmaWx0ZXJzLmxlbmd0aCA+IDApXHJcbiAgICAgICAgYWRkRWxlbWVudE92ZXJsYXkoZWxlbWVudCk7XHJcbiAgICB9XHJcbiAgKTtcclxuXHJcbiAgZG9jdW1lbnQuYWRkRXZlbnRMaXN0ZW5lcihcIm1vdXNlZG93blwiLCBzdG9wRXZlbnRQcm9wYWdhdGlvbiwgdHJ1ZSk7XHJcbiAgZG9jdW1lbnQuYWRkRXZlbnRMaXN0ZW5lcihcIm1vdXNldXBcIiwgc3RvcEV2ZW50UHJvcGFnYXRpb24sIHRydWUpO1xyXG4gIGRvY3VtZW50LmFkZEV2ZW50TGlzdGVuZXIoXCJtb3VzZWVudGVyXCIsIHN0b3BFdmVudFByb3BhZ2F0aW9uLCB0cnVlKTtcclxuICBkb2N1bWVudC5hZGRFdmVudExpc3RlbmVyKFwibW91c2VsZWF2ZVwiLCBzdG9wRXZlbnRQcm9wYWdhdGlvbiwgdHJ1ZSk7XHJcbiAgZG9jdW1lbnQuYWRkRXZlbnRMaXN0ZW5lcihcIm1vdXNlb3ZlclwiLCBtb3VzZU92ZXIsIHRydWUpO1xyXG4gIGRvY3VtZW50LmFkZEV2ZW50TGlzdGVuZXIoXCJtb3VzZW91dFwiLCBtb3VzZU91dCwgdHJ1ZSk7XHJcbiAgZG9jdW1lbnQuYWRkRXZlbnRMaXN0ZW5lcihcImNsaWNrXCIsIGVsZW1lbnRQaWNrZWQsIHRydWUpO1xyXG4gIGRvY3VtZW50LmFkZEV2ZW50TGlzdGVuZXIoXCJjb250ZXh0bWVudVwiLCBlbGVtZW50UGlja2VkLCB0cnVlKTtcclxuICBkb2N1bWVudC5hZGRFdmVudExpc3RlbmVyKFwia2V5ZG93blwiLCBrZXlEb3duLCB0cnVlKTtcclxuXHJcbiAgZXh0Lm9uRXh0ZW5zaW9uVW5sb2FkZWQuYWRkTGlzdGVuZXIoZGVhY3RpdmF0ZUJsb2NrRWxlbWVudCk7XHJcbn1cclxuXHJcbi8vIFVzZWQgdG8gaGlkZS9zaG93IGJsb2NrZWQgZWxlbWVudHMgb24gY29tcG9zZXIuY29udGVudC5wcmV2aWV3XHJcbmFzeW5jIGZ1bmN0aW9uIHByZXZpZXdCbG9ja2VkRWxlbWVudHMoYWN0aXZlKVxyXG57XHJcbiAgaWYgKCFjdXJyZW50RWxlbWVudClcclxuICAgIHJldHVybjtcclxuXHJcbiAgbGV0IGVsZW1lbnQgPSBjdXJyZW50RWxlbWVudC5wcmlzb25lciB8fCBjdXJyZW50RWxlbWVudDtcclxuICBsZXQgb3ZlcmxheXMgPSBkb2N1bWVudC5xdWVyeVNlbGVjdG9yQWxsKFwiLl9fYWRibG9ja3BsdXNfX292ZXJsYXlcIik7XHJcblxyXG4gIHByZXZpZXdCbG9ja2VkRWxlbWVudChlbGVtZW50LCBhY3RpdmUsIG92ZXJsYXlzKTtcclxuXHJcbiAgbGV0IHtzZWxlY3RvcnN9ID0gYXdhaXQgZ2V0RmlsdGVyc0ZvckVsZW1lbnQoZWxlbWVudCk7XHJcbiAgaWYgKHNlbGVjdG9ycy5sZW5ndGggPiAwKVxyXG4gIHtcclxuICAgIGxldCBjc3NRdWVyeSA9IHNlbGVjdG9ycy5qb2luKFwiLFwiKTtcclxuICAgIGZvciAobGV0IG5vZGUgb2YgZG9jdW1lbnQucXVlcnlTZWxlY3RvckFsbChjc3NRdWVyeSkpXHJcbiAgICAgIHByZXZpZXdCbG9ja2VkRWxlbWVudChub2RlLCBhY3RpdmUsIG92ZXJsYXlzKTtcclxuICB9XHJcbn1cclxuXHJcbi8vIHRoZSBwcmV2aWV3QmxvY2tlZEVsZW1lbnRzIGhlbHBlciB0byBhdm9pZCBkdXBsaWNhdGVkIGNvZGVcclxuZnVuY3Rpb24gcHJldmlld0Jsb2NrZWRFbGVtZW50KGVsZW1lbnQsIGFjdGl2ZSwgb3ZlcmxheXMpXHJcbntcclxuICBsZXQgZGlzcGxheSA9IGFjdGl2ZSA/IFwibm9uZVwiIDogbnVsbDtcclxuICBsZXQgZmluZCA9IEFycmF5LnByb3RvdHlwZS5maW5kO1xyXG4gIGxldCBvdmVybGF5ID0gZmluZC5jYWxsKG92ZXJsYXlzLCAoe3ByaXNvbmVyfSkgPT4gcHJpc29uZXIgPT09IGVsZW1lbnQpO1xyXG4gIGlmIChvdmVybGF5KVxyXG4gICAgb3ZlcmxheS5zdHlsZS5kaXNwbGF5ID0gZGlzcGxheTtcclxuICBlbGVtZW50LnN0eWxlLmRpc3BsYXkgPSBkaXNwbGF5O1xyXG59XHJcblxyXG4vLyBUaGUgdXNlciBoYXMgcGlja2VkIGFuIGVsZW1lbnQgLSBjdXJyZW50RWxlbWVudC4gSGlnaGxpZ2h0IGl0IHJlZCwgZ2VuZXJhdGVcclxuLy8gZmlsdGVycyBmb3IgaXQgYW5kIG9wZW4gYSBwb3B1cCBkaWFsb2cgc28gdGhhdCB0aGUgdXNlciBjYW4gY29uZmlybS5cclxuYXN5bmMgZnVuY3Rpb24gZWxlbWVudFBpY2tlZChldmVudClcclxue1xyXG4gIGlmICghY3VycmVudEVsZW1lbnQpXHJcbiAgICByZXR1cm47XHJcblxyXG4gIGV2ZW50LnByZXZlbnREZWZhdWx0KCk7XHJcbiAgZXZlbnQuc3RvcFByb3BhZ2F0aW9uKCk7XHJcblxyXG4gIGxldCBlbGVtZW50ID0gY3VycmVudEVsZW1lbnQucHJpc29uZXIgfHwgY3VycmVudEVsZW1lbnQ7XHJcbiAgbGV0IHtmaWx0ZXJzLCBzZWxlY3RvcnN9ID0gYXdhaXQgZ2V0RmlsdGVyc0ZvckVsZW1lbnQoZWxlbWVudCk7XHJcbiAgaWYgKGN1cnJlbnRseVBpY2tpbmdFbGVtZW50KVxyXG4gICAgc3RvcFBpY2tpbmdFbGVtZW50KCk7XHJcblxyXG4gIGhpZ2hsaWdodEVsZW1lbnQoY3VycmVudEVsZW1lbnQsIFwiI0NBMDAwMFwiLCBcIiNDQTAwMDBcIik7XHJcblxyXG4gIGxldCBoaWdobGlnaHRzID0gMTtcclxuICBpZiAoc2VsZWN0b3JzLmxlbmd0aCA+IDApXHJcbiAge1xyXG4gICAgbGV0IGNzc1F1ZXJ5ID0gc2VsZWN0b3JzLmpvaW4oXCIsXCIpO1xyXG4gICAgaGlnaGxpZ2h0RWxlbWVudHMoY3NzUXVlcnkpO1xyXG4gICAgaGlnaGxpZ2h0cyA9IGRvY3VtZW50LnF1ZXJ5U2VsZWN0b3JBbGwoY3NzUXVlcnkpLmxlbmd0aDtcclxuICB9XHJcblxyXG4gIGxldCBwb3B1cElkID0gYXdhaXQgYnJvd3Nlci5ydW50aW1lLnNlbmRNZXNzYWdlKHtcclxuICAgIHR5cGU6IFwiY29tcG9zZXIub3BlbkRpYWxvZ1wiLFxyXG4gICAgZmlsdGVycyxcclxuICAgIGhpZ2hsaWdodHNcclxuICB9KTtcclxuICAvLyBPbmx5IHRoZSB0b3AgZnJhbWUga2VlcHMgYSByZWNvcmQgb2YgdGhlIHBvcHVwIHdpbmRvdydzIElELFxyXG4gIC8vIHNvIGlmIHRoaXMgaXNuJ3QgdGhlIHRvcCBmcmFtZSB3ZSBuZWVkIHRvIHBhc3MgdGhlIElEIG9uLlxyXG4gIGlmICh3aW5kb3cgPT0gd2luZG93LnRvcClcclxuICB7XHJcbiAgICBibG9ja2VsZW1lbnRQb3B1cElkID0gcG9wdXBJZDtcclxuICB9XHJcbiAgZWxzZVxyXG4gIHtcclxuICAgIGJyb3dzZXIucnVudGltZS5zZW5kTWVzc2FnZSh7XHJcbiAgICAgIHR5cGU6IFwiY29tcG9zZXIuZm9yd2FyZFwiLFxyXG4gICAgICBwYXlsb2FkOiB7dHlwZTogXCJjb21wb3Nlci5jb250ZW50LmRpYWxvZ09wZW5lZFwiLCBwb3B1cElkfVxyXG4gICAgfSk7XHJcbiAgfVxyXG59XHJcblxyXG5mdW5jdGlvbiBzdG9wUGlja2luZ0VsZW1lbnQoKVxyXG57XHJcbiAgY3VycmVudGx5UGlja2luZ0VsZW1lbnQgPSBmYWxzZTtcclxuXHJcbiAgZG9jdW1lbnQucmVtb3ZlRXZlbnRMaXN0ZW5lcihcIm1vdXNlZG93blwiLCBzdG9wRXZlbnRQcm9wYWdhdGlvbiwgdHJ1ZSk7XHJcbiAgZG9jdW1lbnQucmVtb3ZlRXZlbnRMaXN0ZW5lcihcIm1vdXNldXBcIiwgc3RvcEV2ZW50UHJvcGFnYXRpb24sIHRydWUpO1xyXG4gIGRvY3VtZW50LnJlbW92ZUV2ZW50TGlzdGVuZXIoXCJtb3VzZWVudGVyXCIsIHN0b3BFdmVudFByb3BhZ2F0aW9uLCB0cnVlKTtcclxuICBkb2N1bWVudC5yZW1vdmVFdmVudExpc3RlbmVyKFwibW91c2VsZWF2ZVwiLCBzdG9wRXZlbnRQcm9wYWdhdGlvbiwgdHJ1ZSk7XHJcbiAgZG9jdW1lbnQucmVtb3ZlRXZlbnRMaXN0ZW5lcihcIm1vdXNlb3ZlclwiLCBtb3VzZU92ZXIsIHRydWUpO1xyXG4gIGRvY3VtZW50LnJlbW92ZUV2ZW50TGlzdGVuZXIoXCJtb3VzZW91dFwiLCBtb3VzZU91dCwgdHJ1ZSk7XHJcbiAgZG9jdW1lbnQucmVtb3ZlRXZlbnRMaXN0ZW5lcihcImNsaWNrXCIsIGVsZW1lbnRQaWNrZWQsIHRydWUpO1xyXG4gIGRvY3VtZW50LnJlbW92ZUV2ZW50TGlzdGVuZXIoXCJjb250ZXh0bWVudVwiLCBlbGVtZW50UGlja2VkLCB0cnVlKTtcclxuICBkb2N1bWVudC5yZW1vdmVFdmVudExpc3RlbmVyKFwia2V5ZG93blwiLCBrZXlEb3duLCB0cnVlKTtcclxufVxyXG5cclxuXHJcbi8qIENvcmUgbG9naWMgKi9cclxuXHJcbi8vIFdlJ3JlIGRvbmUgd2l0aCB0aGUgYmxvY2sgZWxlbWVudCBmZWF0dXJlIGZvciBub3csIHRpZHkgZXZlcnl0aGluZyB1cC5cclxuZnVuY3Rpb24gZGVhY3RpdmF0ZUJsb2NrRWxlbWVudChwb3B1cEFscmVhZHlDbG9zZWQpXHJcbntcclxuICBwcmV2aWV3QmxvY2tlZEVsZW1lbnRzKGZhbHNlKTtcclxuXHJcbiAgaWYgKGN1cnJlbnRseVBpY2tpbmdFbGVtZW50KVxyXG4gICAgc3RvcFBpY2tpbmdFbGVtZW50KCk7XHJcblxyXG4gIGlmIChibG9ja2VsZW1lbnRQb3B1cElkICE9IG51bGwgJiYgIXBvcHVwQWxyZWFkeUNsb3NlZClcclxuICB7XHJcbiAgICBicm93c2VyLnJ1bnRpbWUuc2VuZE1lc3NhZ2Uoe1xyXG4gICAgICB0eXBlOiBcImNvbXBvc2VyLmZvcndhcmRcIixcclxuICAgICAgdGFyZ2V0UGFnZUlkOiBibG9ja2VsZW1lbnRQb3B1cElkLFxyXG4gICAgICBwYXlsb2FkOlxyXG4gICAgICB7XHJcbiAgICAgICAgdHlwZTogXCJjb21wb3Nlci5kaWFsb2cuY2xvc2VcIlxyXG4gICAgICB9XHJcbiAgICB9KTtcclxuICB9XHJcblxyXG4gIGJsb2NrZWxlbWVudFBvcHVwSWQgPSBudWxsO1xyXG4gIGxhc3RSaWdodENsaWNrRXZlbnQgPSBudWxsO1xyXG5cclxuICBpZiAoY3VycmVudEVsZW1lbnQpXHJcbiAge1xyXG4gICAgdW5oaWdobGlnaHRFbGVtZW50KGN1cnJlbnRFbGVtZW50KTtcclxuICAgIGN1cnJlbnRFbGVtZW50ID0gbnVsbDtcclxuICB9XHJcbiAgdW5oaWdobGlnaHRFbGVtZW50cygpO1xyXG5cclxuICBsZXQgb3ZlcmxheXMgPSBkb2N1bWVudC5nZXRFbGVtZW50c0J5Q2xhc3NOYW1lKFwiX19hZGJsb2NrcGx1c19fb3ZlcmxheVwiKTtcclxuICB3aGlsZSAob3ZlcmxheXMubGVuZ3RoID4gMClcclxuICAgIG92ZXJsYXlzWzBdLnBhcmVudE5vZGUucmVtb3ZlQ2hpbGQob3ZlcmxheXNbMF0pO1xyXG5cclxuICBleHQub25FeHRlbnNpb25VbmxvYWRlZC5yZW1vdmVMaXN0ZW5lcihkZWFjdGl2YXRlQmxvY2tFbGVtZW50KTtcclxufVxyXG5cclxuZnVuY3Rpb24gaW5pdGlhbGl6ZUNvbXBvc2VyKClcclxue1xyXG4gIC8vIFVzZSBhIGNvbnRleHRtZW51IGhhbmRsZXIgdG8gc2F2ZSB0aGUgbGFzdCBlbGVtZW50IHRoZSB1c2VyIHJpZ2h0LWNsaWNrZWRcclxuICAvLyBvbi4gVG8gbWFrZSB0aGluZ3MgZWFzaWVyLCB3ZSBhY3R1YWxseSBzYXZlIHRoZSBET00gZXZlbnQuIFdlIGhhdmUgdG8gZG9cclxuICAvLyB0aGlzIGJlY2F1c2UgdGhlIGNvbnRleHRNZW51IEFQSSBvbmx5IHByb3ZpZGVzIGEgVVJMLCBub3QgdGhlIGFjdHVhbCBET01cclxuICAvLyBlbGVtZW50LlxyXG4gIC8vICAgV2UgYWxzbyBuZWVkIHRvIG1ha2Ugc3VyZSB0aGF0IHRoZSBwcmV2aW91cyByaWdodCBjbGljayBldmVudCxcclxuICAvLyBpZiB0aGVyZSBpcyBvbmUsIGlzIHJlbW92ZWQuIFdlIGRvbid0IGtub3cgd2hpY2ggZnJhbWUgaXQgaXMgaW4gc28gd2UgbXVzdFxyXG4gIC8vIHNlbmQgYSBtZXNzYWdlIHRvIHRoZSBvdGhlciBmcmFtZXMgdG8gY2xlYXIgdGhlaXIgb2xkIHJpZ2h0IGNsaWNrIGV2ZW50cy5cclxuICBkb2N1bWVudC5hZGRFdmVudExpc3RlbmVyKFwiY29udGV4dG1lbnVcIiwgZXZlbnQgPT5cclxuICB7XHJcbiAgICBsYXN0UmlnaHRDbGlja0V2ZW50ID0gZXZlbnQ7XHJcbiAgICBsYXN0UmlnaHRDbGlja0V2ZW50SXNNb3N0UmVjZW50ID0gdHJ1ZTtcclxuXHJcbiAgICBicm93c2VyLnJ1bnRpbWUuc2VuZE1lc3NhZ2Uoe1xyXG4gICAgICB0eXBlOiBcImNvbXBvc2VyLmZvcndhcmRcIixcclxuICAgICAgcGF5bG9hZDpcclxuICAgICAge1xyXG4gICAgICAgIHR5cGU6IFwiY29tcG9zZXIuY29udGVudC5jbGVhclByZXZpb3VzUmlnaHRDbGlja0V2ZW50XCJcclxuICAgICAgfVxyXG4gICAgfSk7XHJcbiAgfSwgdHJ1ZSk7XHJcblxyXG4gIGV4dC5vbk1lc3NhZ2UuYWRkTGlzdGVuZXIoYXN5bmMobWVzc2FnZSwgc2VuZGVyLCBzZW5kUmVzcG9uc2UpID0+XHJcbiAge1xyXG4gICAgc3dpdGNoIChtZXNzYWdlLnR5cGUpXHJcbiAgICB7XHJcbiAgICAgIGNhc2UgXCJjb21wb3Nlci5jb250ZW50LnByZXZpZXdcIjpcclxuICAgICAgICBwcmV2aWV3QmxvY2tlZEVsZW1lbnRzKG1lc3NhZ2UuYWN0aXZlKTtcclxuICAgICAgICBicmVhaztcclxuICAgICAgY2FzZSBcImNvbXBvc2VyLmNvbnRlbnQuZ2V0U3RhdGVcIjpcclxuICAgICAgICBpZiAod2luZG93ID09IHdpbmRvdy50b3ApXHJcbiAgICAgICAge1xyXG4gICAgICAgICAgc2VuZFJlc3BvbnNlKHtcclxuICAgICAgICAgICAgYWN0aXZlOiBjdXJyZW50bHlQaWNraW5nRWxlbWVudCB8fCBibG9ja2VsZW1lbnRQb3B1cElkICE9IG51bGxcclxuICAgICAgICAgIH0pO1xyXG4gICAgICAgIH1cclxuICAgICAgICBicmVhaztcclxuICAgICAgY2FzZSBcImNvbXBvc2VyLmNvbnRlbnQuc3RhcnRQaWNraW5nRWxlbWVudFwiOlxyXG4gICAgICAgIGlmICh3aW5kb3cgPT0gd2luZG93LnRvcClcclxuICAgICAgICAgIHN0YXJ0UGlja2luZ0VsZW1lbnQoKTtcclxuICAgICAgICBicmVhaztcclxuICAgICAgY2FzZSBcImNvbXBvc2VyLmNvbnRlbnQuY29udGV4dE1lbnVDbGlja2VkXCI6XHJcbiAgICAgICAgbGV0IGV2ZW50ID0gbGFzdFJpZ2h0Q2xpY2tFdmVudDtcclxuICAgICAgICBkZWFjdGl2YXRlQmxvY2tFbGVtZW50KCk7XHJcbiAgICAgICAgaWYgKGV2ZW50KVxyXG4gICAgICAgIHtcclxuICAgICAgICAgIGdldEJsb2NrYWJsZUVsZW1lbnRPckFuY2VzdG9yKGV2ZW50LnRhcmdldCkudGhlbihlbGVtZW50ID0+XHJcbiAgICAgICAgICB7XHJcbiAgICAgICAgICAgIGlmIChlbGVtZW50KVxyXG4gICAgICAgICAgICB7XHJcbiAgICAgICAgICAgICAgY3VycmVudEVsZW1lbnQgPSBlbGVtZW50O1xyXG4gICAgICAgICAgICAgIGVsZW1lbnRQaWNrZWQoZXZlbnQpO1xyXG4gICAgICAgICAgICB9XHJcbiAgICAgICAgICB9KTtcclxuICAgICAgICB9XHJcbiAgICAgICAgYnJlYWs7XHJcbiAgICAgIGNhc2UgXCJjb21wb3Nlci5jb250ZW50LmZpbmlzaGVkXCI6XHJcbiAgICAgICAgaWYgKGN1cnJlbnRFbGVtZW50ICYmIG1lc3NhZ2UucmVtb3ZlKVxyXG4gICAgICAgIHtcclxuICAgICAgICAgIC8vIEhpZGUgdGhlIHNlbGVjdGVkIGVsZW1lbnQgaXRzZWxmLiBOb3RlIHRoYXQgdGhpc1xyXG4gICAgICAgICAgLy8gYmVoYXZpb3IgaXMgaW5jb21wbGV0ZSwgYnV0IHRoZSBiZXN0IHdlIGNhbiBkbyBoZXJlLFxyXG4gICAgICAgICAgLy8gZS5nLiBpZiBhbiBhZGRlZCBibG9ja2luZyBmaWx0ZXIgbWF0Y2hlcyBvdGhlciBlbGVtZW50cyxcclxuICAgICAgICAgIC8vIHRoZSBlZmZlY3Qgd29uJ3QgYmUgdmlzaWJsZSB1bnRpbCB0aGUgcGFnZSBpcyBpcyByZWxvYWRlZC5cclxuICAgICAgICAgIGNvbGxhcHNlRWxlbWVudChjdXJyZW50RWxlbWVudC5wcmlzb25lciB8fCBjdXJyZW50RWxlbWVudCk7XHJcblxyXG4gICAgICAgICAgLy8gQXBwbHkgYWRkZWQgZWxlbWVudCBoaWRpbmcgZmlsdGVycy5cclxuICAgICAgICAgIGNvbnRlbnRGaWx0ZXJpbmcuYXBwbHkoe2VsZW1oaWRlOiB0cnVlfSk7XHJcbiAgICAgICAgfVxyXG4gICAgICAgIGRlYWN0aXZhdGVCbG9ja0VsZW1lbnQoISFtZXNzYWdlLnBvcHVwQWxyZWFkeUNsb3NlZCk7XHJcbiAgICAgICAgYnJlYWs7XHJcbiAgICAgIGNhc2UgXCJjb21wb3Nlci5jb250ZW50LmNsZWFyUHJldmlvdXNSaWdodENsaWNrRXZlbnRcIjpcclxuICAgICAgICBpZiAoIWxhc3RSaWdodENsaWNrRXZlbnRJc01vc3RSZWNlbnQpXHJcbiAgICAgICAgICBsYXN0UmlnaHRDbGlja0V2ZW50ID0gbnVsbDtcclxuICAgICAgICBsYXN0UmlnaHRDbGlja0V2ZW50SXNNb3N0UmVjZW50ID0gZmFsc2U7XHJcbiAgICAgICAgYnJlYWs7XHJcbiAgICAgIGNhc2UgXCJjb21wb3Nlci5jb250ZW50LmRpYWxvZ09wZW5lZFwiOlxyXG4gICAgICAgIGlmICh3aW5kb3cgPT0gd2luZG93LnRvcClcclxuICAgICAgICAgIGJsb2NrZWxlbWVudFBvcHVwSWQgPSBtZXNzYWdlLnBvcHVwSWQ7XHJcbiAgICAgICAgYnJlYWs7XHJcbiAgICAgIGNhc2UgXCJjb21wb3Nlci5jb250ZW50LmRpYWxvZ0Nsb3NlZFwiOlxyXG4gICAgICAgIC8vIFRoZSBvblJlbW92ZWQgaG9vayBmb3IgdGhlIHBvcHVwIGNhbiBjcmVhdGUgYSByYWNlIGNvbmRpdGlvbiwgc28gd2VcclxuICAgICAgICAvLyB0byBiZSBjYXJlZnVsIGhlcmUuIChUaGlzIGlzIG5vdCBwZXJmZWN0LCBidXQgYmVzdCB3ZSBjYW4gZG8uKVxyXG4gICAgICAgIGlmICh3aW5kb3cgPT0gd2luZG93LnRvcCAmJiBibG9ja2VsZW1lbnRQb3B1cElkID09IG1lc3NhZ2UucG9wdXBJZClcclxuICAgICAgICB7XHJcbiAgICAgICAgICBicm93c2VyLnJ1bnRpbWUuc2VuZE1lc3NhZ2Uoe1xyXG4gICAgICAgICAgICB0eXBlOiBcImNvbXBvc2VyLmZvcndhcmRcIixcclxuICAgICAgICAgICAgcGF5bG9hZDpcclxuICAgICAgICAgICAge1xyXG4gICAgICAgICAgICAgIHR5cGU6IFwiY29tcG9zZXIuY29udGVudC5maW5pc2hlZFwiLFxyXG4gICAgICAgICAgICAgIHBvcHVwQWxyZWFkeUNsb3NlZDogdHJ1ZVxyXG4gICAgICAgICAgICB9XHJcbiAgICAgICAgICB9KTtcclxuICAgICAgICB9XHJcbiAgICAgICAgYnJlYWs7XHJcbiAgICB9XHJcbiAgfSk7XHJcblxyXG4gIGlmICh3aW5kb3cgPT0gd2luZG93LnRvcClcclxuICAgIGJyb3dzZXIucnVudGltZS5zZW5kTWVzc2FnZSh7dHlwZTogXCJjb21wb3Nlci5yZWFkeVwifSk7XHJcbn1cclxuXHJcbmlmIChkb2N1bWVudCBpbnN0YW5jZW9mIEhUTUxEb2N1bWVudClcclxuICBpbml0aWFsaXplQ29tcG9zZXIoKTtcclxuIiwiLypcclxuICogVGhpcyBmaWxlIGlzIHBhcnQgb2YgQWRibG9jayBQbHVzIDxodHRwczovL2FkYmxvY2twbHVzLm9yZy8+LFxyXG4gKiBDb3B5cmlnaHQgKEMpIDIwMDYtcHJlc2VudCBleWVvIEdtYkhcclxuICpcclxuICogQWRibG9jayBQbHVzIGlzIGZyZWUgc29mdHdhcmU6IHlvdSBjYW4gcmVkaXN0cmlidXRlIGl0IGFuZC9vciBtb2RpZnlcclxuICogaXQgdW5kZXIgdGhlIHRlcm1zIG9mIHRoZSBHTlUgR2VuZXJhbCBQdWJsaWMgTGljZW5zZSB2ZXJzaW9uIDMgYXNcclxuICogcHVibGlzaGVkIGJ5IHRoZSBGcmVlIFNvZnR3YXJlIEZvdW5kYXRpb24uXHJcbiAqXHJcbiAqIEFkYmxvY2sgUGx1cyBpcyBkaXN0cmlidXRlZCBpbiB0aGUgaG9wZSB0aGF0IGl0IHdpbGwgYmUgdXNlZnVsLFxyXG4gKiBidXQgV0lUSE9VVCBBTlkgV0FSUkFOVFk7IHdpdGhvdXQgZXZlbiB0aGUgaW1wbGllZCB3YXJyYW50eSBvZlxyXG4gKiBNRVJDSEFOVEFCSUxJVFkgb3IgRklUTkVTUyBGT1IgQSBQQVJUSUNVTEFSIFBVUlBPU0UuICBTZWUgdGhlXHJcbiAqIEdOVSBHZW5lcmFsIFB1YmxpYyBMaWNlbnNlIGZvciBtb3JlIGRldGFpbHMuXHJcbiAqXHJcbiAqIFlvdSBzaG91bGQgaGF2ZSByZWNlaXZlZCBhIGNvcHkgb2YgdGhlIEdOVSBHZW5lcmFsIFB1YmxpYyBMaWNlbnNlXHJcbiAqIGFsb25nIHdpdGggQWRibG9jayBQbHVzLiAgSWYgbm90LCBzZWUgPGh0dHA6Ly93d3cuZ251Lm9yZy9saWNlbnNlcy8+LlxyXG4gKi9cclxuXHJcbmltcG9ydCB7RWxlbUhpZGVFbXVsYXRpb259XHJcbiAgZnJvbSBcIi4vYWRibG9ja3BsdXNjb3JlL2xpYi9jb250ZW50L2VsZW1IaWRlRW11bGF0aW9uLmpzXCI7XHJcblxyXG5leHBvcnQgbGV0IGNvbnRlbnRGaWx0ZXJpbmc7XHJcbmxldCBjb2xsYXBzZWRTZWxlY3RvcnMgPSBuZXcgU2V0KCk7XHJcblxyXG5leHBvcnQgZnVuY3Rpb24gZ2V0VVJMRnJvbUVsZW1lbnQoZWxlbWVudClcclxue1xyXG4gIGlmIChlbGVtZW50LmxvY2FsTmFtZSA9PSBcIm9iamVjdFwiKVxyXG4gIHtcclxuICAgIGlmIChlbGVtZW50LmRhdGEpXHJcbiAgICAgIHJldHVybiBlbGVtZW50LmRhdGE7XHJcblxyXG4gICAgZm9yIChsZXQgY2hpbGQgb2YgZWxlbWVudC5jaGlsZHJlbilcclxuICAgIHtcclxuICAgICAgaWYgKGNoaWxkLmxvY2FsTmFtZSA9PSBcInBhcmFtXCIgJiYgY2hpbGQubmFtZSA9PSBcIm1vdmllXCIgJiYgY2hpbGQudmFsdWUpXHJcbiAgICAgICAgcmV0dXJuIG5ldyBVUkwoY2hpbGQudmFsdWUsIGRvY3VtZW50LmJhc2VVUkkpLmhyZWY7XHJcbiAgICB9XHJcblxyXG4gICAgcmV0dXJuIG51bGw7XHJcbiAgfVxyXG5cclxuICByZXR1cm4gZWxlbWVudC5jdXJyZW50U3JjIHx8IGVsZW1lbnQuc3JjO1xyXG59XHJcblxyXG5mdW5jdGlvbiBnZXRTZWxlY3RvckZvckJsb2NrZWRFbGVtZW50KGVsZW1lbnQpXHJcbntcclxuICAvLyBTZXR0aW5nIHRoZSBcImRpc3BsYXlcIiBDU1MgcHJvcGVydHkgdG8gXCJub25lXCIgZG9lc24ndCBoYXZlIGFueSBlZmZlY3Qgb25cclxuICAvLyA8ZnJhbWU+IGVsZW1lbnRzIChpbiBmcmFtZXNldHMpLiBTbyB3ZSBoYXZlIHRvIGhpZGUgaXQgaW5saW5lIHRocm91Z2hcclxuICAvLyB0aGUgXCJ2aXNpYmlsaXR5XCIgQ1NTIHByb3BlcnR5LlxyXG4gIGlmIChlbGVtZW50LmxvY2FsTmFtZSA9PSBcImZyYW1lXCIpXHJcbiAgICByZXR1cm4gbnVsbDtcclxuXHJcbiAgLy8gSWYgdGhlIDx2aWRlbz4gb3IgPGF1ZGlvPiBlbGVtZW50IGNvbnRhaW5zIGFueSA8c291cmNlPiBjaGlsZHJlbixcclxuICAvLyB3ZSBjYW5ub3QgYWRkcmVzcyBpdCBpbiBDU1MgYnkgdGhlIHNvdXJjZSBVUkw7IGluIHRoYXQgY2FzZSB3ZVxyXG4gIC8vIGRvbid0IFwiY29sbGFwc2VcIiBpdCB1c2luZyBhIENTUyBzZWxlY3RvciBidXQgcmF0aGVyIGhpZGUgaXQgZGlyZWN0bHkgYnlcclxuICAvLyBzZXR0aW5nIHRoZSBzdHlsZT1cIi4uLlwiIGF0dHJpYnV0ZS5cclxuICBpZiAoZWxlbWVudC5sb2NhbE5hbWUgPT0gXCJ2aWRlb1wiIHx8IGVsZW1lbnQubG9jYWxOYW1lID09IFwiYXVkaW9cIilcclxuICB7XHJcbiAgICBmb3IgKGxldCBjaGlsZCBvZiBlbGVtZW50LmNoaWxkcmVuKVxyXG4gICAge1xyXG4gICAgICBpZiAoY2hpbGQubG9jYWxOYW1lID09IFwic291cmNlXCIpXHJcbiAgICAgICAgcmV0dXJuIG51bGw7XHJcbiAgICB9XHJcbiAgfVxyXG5cclxuICBsZXQgc2VsZWN0b3IgPSBcIlwiO1xyXG4gIGZvciAobGV0IGF0dHIgb2YgW1wic3JjXCIsIFwic3Jjc2V0XCJdKVxyXG4gIHtcclxuICAgIGxldCB2YWx1ZSA9IGVsZW1lbnQuZ2V0QXR0cmlidXRlKGF0dHIpO1xyXG4gICAgaWYgKHZhbHVlICYmIGF0dHIgaW4gZWxlbWVudClcclxuICAgICAgc2VsZWN0b3IgKz0gXCJbXCIgKyBhdHRyICsgXCI9XCIgKyBDU1MuZXNjYXBlKHZhbHVlKSArIFwiXVwiO1xyXG4gIH1cclxuXHJcbiAgcmV0dXJuIHNlbGVjdG9yID8gZWxlbWVudC5sb2NhbE5hbWUgKyBzZWxlY3RvciA6IG51bGw7XHJcbn1cclxuXHJcbmZ1bmN0aW9uIGhpZGVFbGVtZW50KGVsZW1lbnQsIHByb3BlcnRpZXMpXHJcbntcclxuICBsZXQge3N0eWxlfSA9IGVsZW1lbnQ7XHJcbiAgbGV0IGFjdHVhbFByb3BlcnRpZXMgPSBbXTtcclxuXHJcbiAgaWYgKGVsZW1lbnQubG9jYWxOYW1lID09IFwiZnJhbWVcIilcclxuICAgIGFjdHVhbFByb3BlcnRpZXMgPSBwcm9wZXJ0aWVzID0gW1tcInZpc2liaWxpdHlcIiwgXCJoaWRkZW5cIl1dO1xyXG4gIGVsc2UgaWYgKCFwcm9wZXJ0aWVzKVxyXG4gICAgYWN0dWFsUHJvcGVydGllcyA9IHByb3BlcnRpZXMgPSBbW1wiZGlzcGxheVwiLCBcIm5vbmVcIl1dO1xyXG5cclxuICBmb3IgKGxldCBba2V5LCB2YWx1ZV0gb2YgcHJvcGVydGllcylcclxuICAgIHN0eWxlLnNldFByb3BlcnR5KGtleSwgdmFsdWUsIFwiaW1wb3J0YW50XCIpO1xyXG5cclxuICBpZiAoIWFjdHVhbFByb3BlcnRpZXMpXHJcbiAge1xyXG4gICAgYWN0dWFsUHJvcGVydGllcyA9IFtdO1xyXG4gICAgZm9yIChsZXQgW2tleV0gb2YgcHJvcGVydGllcylcclxuICAgICAgYWN0dWFsUHJvcGVydGllcy5wdXNoKFtrZXksIHN0eWxlLmdldFByb3BlcnR5VmFsdWUoa2V5KV0pO1xyXG4gIH1cclxuXHJcbiAgbmV3IE11dGF0aW9uT2JzZXJ2ZXIoKCkgPT5cclxuICB7XHJcbiAgICBmb3IgKGxldCBba2V5LCB2YWx1ZV0gb2YgYWN0dWFsUHJvcGVydGllcylcclxuICAgIHtcclxuICAgICAgaWYgKHN0eWxlLmdldFByb3BlcnR5VmFsdWUoa2V5KSAhPSB2YWx1ZSB8fFxyXG4gICAgICAgICAgc3R5bGUuZ2V0UHJvcGVydHlQcmlvcml0eShrZXkpICE9IFwiaW1wb3J0YW50XCIpXHJcbiAgICAgICAgc3R5bGUuc2V0UHJvcGVydHkoa2V5LCB2YWx1ZSwgXCJpbXBvcnRhbnRcIik7XHJcbiAgICB9XHJcbiAgfSkub2JzZXJ2ZShcclxuICAgIGVsZW1lbnQsIHtcclxuICAgICAgYXR0cmlidXRlczogdHJ1ZSxcclxuICAgICAgYXR0cmlidXRlRmlsdGVyOiBbXCJzdHlsZVwiXVxyXG4gICAgfVxyXG4gICk7XHJcbn1cclxuXHJcbmV4cG9ydCBmdW5jdGlvbiBjb2xsYXBzZUVsZW1lbnQoZWxlbWVudClcclxue1xyXG4gIGxldCBzZWxlY3RvciA9IGdldFNlbGVjdG9yRm9yQmxvY2tlZEVsZW1lbnQoZWxlbWVudCk7XHJcbiAgaWYgKHNlbGVjdG9yKVxyXG4gIHtcclxuICAgIGlmICghY29sbGFwc2VkU2VsZWN0b3JzLmhhcyhzZWxlY3RvcikpXHJcbiAgICB7XHJcbiAgICAgIGNvbnRlbnRGaWx0ZXJpbmcuYWRkU2VsZWN0b3JzKFtzZWxlY3Rvcl0sIFwiY29sbGFwc2luZ1wiLCB0cnVlKTtcclxuICAgICAgY29sbGFwc2VkU2VsZWN0b3JzLmFkZChzZWxlY3Rvcik7XHJcbiAgICB9XHJcbiAgfVxyXG4gIGVsc2VcclxuICB7XHJcbiAgICBoaWRlRWxlbWVudChlbGVtZW50KTtcclxuICB9XHJcbn1cclxuXHJcbmZ1bmN0aW9uIHN0YXJ0RWxlbWVudENvbGxhcHNpbmcoKVxyXG57XHJcbiAgbGV0IGRlZmVycmVkID0gbnVsbDtcclxuXHJcbiAgYnJvd3Nlci5ydW50aW1lLm9uTWVzc2FnZS5hZGRMaXN0ZW5lcigobWVzc2FnZSwgc2VuZGVyKSA9PlxyXG4gIHtcclxuICAgIGlmIChtZXNzYWdlLnR5cGUgIT0gXCJmaWx0ZXJzLmNvbGxhcHNlXCIpXHJcbiAgICAgIHJldHVybjtcclxuXHJcbiAgICBpZiAoZG9jdW1lbnQucmVhZHlTdGF0ZSA9PSBcImxvYWRpbmdcIilcclxuICAgIHtcclxuICAgICAgaWYgKCFkZWZlcnJlZClcclxuICAgICAge1xyXG4gICAgICAgIGRlZmVycmVkID0gbmV3IE1hcCgpO1xyXG4gICAgICAgIGRvY3VtZW50LmFkZEV2ZW50TGlzdGVuZXIoXCJET01Db250ZW50TG9hZGVkXCIsICgpID0+XHJcbiAgICAgICAge1xyXG4gICAgICAgICAgZm9yIChsZXQgW3NlbGVjdG9yLCB1cmxzXSBvZiBkZWZlcnJlZClcclxuICAgICAgICAgIHtcclxuICAgICAgICAgICAgZm9yIChsZXQgZWxlbWVudCBvZiBkb2N1bWVudC5xdWVyeVNlbGVjdG9yQWxsKHNlbGVjdG9yKSlcclxuICAgICAgICAgICAge1xyXG4gICAgICAgICAgICAgIGlmICh1cmxzLmhhcyhnZXRVUkxGcm9tRWxlbWVudChlbGVtZW50KSkpXHJcbiAgICAgICAgICAgICAgICBjb2xsYXBzZUVsZW1lbnQoZWxlbWVudCk7XHJcbiAgICAgICAgICAgIH1cclxuICAgICAgICAgIH1cclxuXHJcbiAgICAgICAgICBkZWZlcnJlZCA9IG51bGw7XHJcbiAgICAgICAgfSk7XHJcbiAgICAgIH1cclxuXHJcbiAgICAgIGxldCB1cmxzID0gZGVmZXJyZWQuZ2V0KG1lc3NhZ2Uuc2VsZWN0b3IpIHx8IG5ldyBTZXQoKTtcclxuICAgICAgZGVmZXJyZWQuc2V0KG1lc3NhZ2Uuc2VsZWN0b3IsIHVybHMpO1xyXG4gICAgICB1cmxzLmFkZChtZXNzYWdlLnVybCk7XHJcbiAgICB9XHJcbiAgICBlbHNlXHJcbiAgICB7XHJcbiAgICAgIGZvciAobGV0IGVsZW1lbnQgb2YgZG9jdW1lbnQucXVlcnlTZWxlY3RvckFsbChtZXNzYWdlLnNlbGVjdG9yKSlcclxuICAgICAge1xyXG4gICAgICAgIGlmIChnZXRVUkxGcm9tRWxlbWVudChlbGVtZW50KSA9PSBtZXNzYWdlLnVybClcclxuICAgICAgICAgIGNvbGxhcHNlRWxlbWVudChlbGVtZW50KTtcclxuICAgICAgfVxyXG4gICAgfVxyXG4gIH0pO1xyXG59XHJcblxyXG5mdW5jdGlvbiBjaGVja1NpdGVrZXkoKVxyXG57XHJcbiAgbGV0IGF0dHIgPSBkb2N1bWVudC5kb2N1bWVudEVsZW1lbnQuZ2V0QXR0cmlidXRlKFwiZGF0YS1hZGJsb2Nra2V5XCIpO1xyXG4gIGlmIChhdHRyKVxyXG4gICAgYnJvd3Nlci5ydW50aW1lLnNlbmRNZXNzYWdlKHt0eXBlOiBcImZpbHRlcnMuYWRkS2V5XCIsIHRva2VuOiBhdHRyfSk7XHJcbn1cclxuXHJcbmNsYXNzIEVsZW1lbnRIaWRpbmdUcmFjZXJcclxue1xyXG4gIGNvbnN0cnVjdG9yKHNlbGVjdG9ycywgZXhjZXB0aW9ucylcclxuICB7XHJcbiAgICB0aGlzLnNlbGVjdG9ycyA9IHNlbGVjdG9ycztcclxuICAgIHRoaXMuZXhjZXB0aW9ucyA9IGV4Y2VwdGlvbnM7XHJcbiAgICB0aGlzLmNoYW5nZWROb2RlcyA9IFtdO1xyXG4gICAgdGhpcy50aW1lb3V0ID0gbnVsbDtcclxuICAgIHRoaXMub2JzZXJ2ZXIgPSBuZXcgTXV0YXRpb25PYnNlcnZlcih0aGlzLm9ic2VydmUuYmluZCh0aGlzKSk7XHJcbiAgICB0aGlzLnRyYWNlID0gdGhpcy50cmFjZS5iaW5kKHRoaXMpO1xyXG5cclxuICAgIGlmIChkb2N1bWVudC5yZWFkeVN0YXRlID09IFwibG9hZGluZ1wiKVxyXG4gICAgICBkb2N1bWVudC5hZGRFdmVudExpc3RlbmVyKFwiRE9NQ29udGVudExvYWRlZFwiLCB0aGlzLnRyYWNlKTtcclxuICAgIGVsc2VcclxuICAgICAgdGhpcy50cmFjZSgpO1xyXG4gIH1cclxuXHJcbiAgY2hlY2tOb2Rlcyhub2RlcylcclxuICB7XHJcbiAgICBsZXQgZWZmZWN0aXZlU2VsZWN0b3JzID0gW107XHJcbiAgICBsZXQgZWZmZWN0aXZlRXhjZXB0aW9ucyA9IFtdO1xyXG5cclxuICAgIGZvciAobGV0IHNlbGVjdG9yIG9mIHRoaXMuc2VsZWN0b3JzKVxyXG4gICAge1xyXG4gICAgICBmb3IgKGxldCBub2RlIG9mIG5vZGVzKVxyXG4gICAgICB7XHJcbiAgICAgICAgaWYgKG5vZGUucXVlcnlTZWxlY3RvcihzZWxlY3RvcikpXHJcbiAgICAgICAge1xyXG4gICAgICAgICAgZWZmZWN0aXZlU2VsZWN0b3JzLnB1c2goc2VsZWN0b3IpO1xyXG4gICAgICAgICAgYnJlYWs7XHJcbiAgICAgICAgfVxyXG4gICAgICB9XHJcbiAgICB9XHJcblxyXG4gICAgZm9yIChsZXQgZXhjZXB0aW9uIG9mIHRoaXMuZXhjZXB0aW9ucylcclxuICAgIHtcclxuICAgICAgZm9yIChsZXQgbm9kZSBvZiBub2RlcylcclxuICAgICAge1xyXG4gICAgICAgIGlmIChub2RlLnF1ZXJ5U2VsZWN0b3IoZXhjZXB0aW9uLnNlbGVjdG9yKSlcclxuICAgICAgICB7XHJcbiAgICAgICAgICBlZmZlY3RpdmVFeGNlcHRpb25zLnB1c2goZXhjZXB0aW9uLnRleHQpO1xyXG4gICAgICAgICAgYnJlYWs7XHJcbiAgICAgICAgfVxyXG4gICAgICB9XHJcbiAgICB9XHJcblxyXG4gICAgaWYgKGVmZmVjdGl2ZVNlbGVjdG9ycy5sZW5ndGggPiAwIHx8IGVmZmVjdGl2ZUV4Y2VwdGlvbnMubGVuZ3RoID4gMClcclxuICAgIHtcclxuICAgICAgYnJvd3Nlci5ydW50aW1lLnNlbmRNZXNzYWdlKHtcclxuICAgICAgICB0eXBlOiBcImhpdExvZ2dlci50cmFjZUVsZW1IaWRlXCIsXHJcbiAgICAgICAgc2VsZWN0b3JzOiBlZmZlY3RpdmVTZWxlY3RvcnMsXHJcbiAgICAgICAgZmlsdGVyczogZWZmZWN0aXZlRXhjZXB0aW9uc1xyXG4gICAgICB9KTtcclxuICAgIH1cclxuICB9XHJcblxyXG4gIG9uVGltZW91dCgpXHJcbiAge1xyXG4gICAgdGhpcy5jaGVja05vZGVzKHRoaXMuY2hhbmdlZE5vZGVzKTtcclxuICAgIHRoaXMuY2hhbmdlZE5vZGVzID0gW107XHJcbiAgICB0aGlzLnRpbWVvdXQgPSBudWxsO1xyXG4gIH1cclxuXHJcbiAgb2JzZXJ2ZShtdXRhdGlvbnMpXHJcbiAge1xyXG4gICAgLy8gRm9yZ2V0IHByZXZpb3VzbHkgY2hhbmdlZCBub2RlcyB0aGF0IGFyZSBubyBsb25nZXIgaW4gdGhlIERPTS5cclxuICAgIGZvciAobGV0IGkgPSAwOyBpIDwgdGhpcy5jaGFuZ2VkTm9kZXMubGVuZ3RoOyBpKyspXHJcbiAgICB7XHJcbiAgICAgIGlmICghZG9jdW1lbnQuY29udGFpbnModGhpcy5jaGFuZ2VkTm9kZXNbaV0pKVxyXG4gICAgICAgIHRoaXMuY2hhbmdlZE5vZGVzLnNwbGljZShpLS0sIDEpO1xyXG4gICAgfVxyXG5cclxuICAgIGZvciAobGV0IG11dGF0aW9uIG9mIG11dGF0aW9ucylcclxuICAgIHtcclxuICAgICAgbGV0IG5vZGUgPSBtdXRhdGlvbi50YXJnZXQ7XHJcblxyXG4gICAgICAvLyBJZ25vcmUgbXV0YXRpb25zIG9mIG5vZGVzIHRoYXQgYXJlbid0IGluIHRoZSBET00gYW55bW9yZS5cclxuICAgICAgaWYgKCFkb2N1bWVudC5jb250YWlucyhub2RlKSlcclxuICAgICAgICBjb250aW51ZTtcclxuXHJcbiAgICAgIC8vIFNpbmNlIHF1ZXJ5U2VsZWN0b3JBbGwoKSBkb2Vzbid0IGNvbnNpZGVyIHRoZSByb290IGl0c2VsZlxyXG4gICAgICAvLyBhbmQgc2luY2UgQ1NTIHNlbGVjdG9ycyBjYW4gYWxzbyBtYXRjaCBzaWJsaW5ncywgd2UgaGF2ZVxyXG4gICAgICAvLyB0byBjb25zaWRlciB0aGUgcGFyZW50IG5vZGUgZm9yIGF0dHJpYnV0ZSBtdXRhdGlvbnMuXHJcbiAgICAgIGlmIChtdXRhdGlvbi50eXBlID09IFwiYXR0cmlidXRlc1wiKVxyXG4gICAgICAgIG5vZGUgPSBub2RlLnBhcmVudE5vZGU7XHJcblxyXG4gICAgICBsZXQgYWRkTm9kZSA9IHRydWU7XHJcbiAgICAgIGZvciAobGV0IGkgPSAwOyBpIDwgdGhpcy5jaGFuZ2VkTm9kZXMubGVuZ3RoOyBpKyspXHJcbiAgICAgIHtcclxuICAgICAgICBsZXQgcHJldmlvdXNseUNoYW5nZWROb2RlID0gdGhpcy5jaGFuZ2VkTm9kZXNbaV07XHJcblxyXG4gICAgICAgIC8vIElmIHdlIGFyZSBhbHJlYWR5IGdvaW5nIHRvIGNoZWNrIGFuIGFuY2VzdG9yIG9mIHRoaXMgbm9kZSxcclxuICAgICAgICAvLyB3ZSBjYW4gaWdub3JlIHRoaXMgbm9kZSwgc2luY2UgaXQgd2lsbCBiZSBjb25zaWRlcmVkIGFueXdheVxyXG4gICAgICAgIC8vIHdoZW4gY2hlY2tpbmcgb25lIG9mIGl0cyBhbmNlc3RvcnMuXHJcbiAgICAgICAgaWYgKHByZXZpb3VzbHlDaGFuZ2VkTm9kZS5jb250YWlucyhub2RlKSlcclxuICAgICAgICB7XHJcbiAgICAgICAgICBhZGROb2RlID0gZmFsc2U7XHJcbiAgICAgICAgICBicmVhaztcclxuICAgICAgICB9XHJcblxyXG4gICAgICAgIC8vIElmIHRoaXMgbm9kZSBpcyBhbiBhbmNlc3RvciBvZiBhIG5vZGUgdGhhdCBwcmV2aW91c2x5IGNoYW5nZWQsXHJcbiAgICAgICAgLy8gd2UgY2FuIGlnbm9yZSB0aGF0IG5vZGUsIHNpbmNlIGl0IHdpbGwgYmUgY29uc2lkZXJlZCBhbnl3YXlcclxuICAgICAgICAvLyB3aGVuIGNoZWNraW5nIG9uZSBvZiBpdHMgYW5jZXN0b3JzLlxyXG4gICAgICAgIGlmIChub2RlLmNvbnRhaW5zKHByZXZpb3VzbHlDaGFuZ2VkTm9kZSkpXHJcbiAgICAgICAgICB0aGlzLmNoYW5nZWROb2Rlcy5zcGxpY2UoaS0tLCAxKTtcclxuICAgICAgfVxyXG5cclxuICAgICAgaWYgKGFkZE5vZGUpXHJcbiAgICAgICAgdGhpcy5jaGFuZ2VkTm9kZXMucHVzaChub2RlKTtcclxuICAgIH1cclxuXHJcbiAgICAvLyBDaGVjayBvbmx5IG5vZGVzIHdob3NlIGRlc2NlbmRhbnRzIGhhdmUgY2hhbmdlZCwgYW5kIG5vdCBtb3JlIG9mdGVuXHJcbiAgICAvLyB0aGFuIG9uY2UgYSBzZWNvbmQuIE90aGVyd2lzZSBsYXJnZSBwYWdlcyB3aXRoIGEgbG90IG9mIERPTSBtdXRhdGlvbnNcclxuICAgIC8vIChsaWtlIFlvdVR1YmUpIGZyZWV6ZSB3aGVuIHRoZSBkZXZ0b29scyBwYW5lbCBpcyBhY3RpdmUuXHJcbiAgICBpZiAodGhpcy50aW1lb3V0ID09IG51bGwpXHJcbiAgICAgIHRoaXMudGltZW91dCA9IHNldFRpbWVvdXQodGhpcy5vblRpbWVvdXQuYmluZCh0aGlzKSwgMTAwMCk7XHJcbiAgfVxyXG5cclxuICB0cmFjZSgpXHJcbiAge1xyXG4gICAgdGhpcy5jaGVja05vZGVzKFtkb2N1bWVudF0pO1xyXG5cclxuICAgIHRoaXMub2JzZXJ2ZXIub2JzZXJ2ZShcclxuICAgICAgZG9jdW1lbnQsXHJcbiAgICAgIHtcclxuICAgICAgICBjaGlsZExpc3Q6IHRydWUsXHJcbiAgICAgICAgYXR0cmlidXRlczogdHJ1ZSxcclxuICAgICAgICBzdWJ0cmVlOiB0cnVlXHJcbiAgICAgIH1cclxuICAgICk7XHJcbiAgfVxyXG5cclxuICBkaXNjb25uZWN0KClcclxuICB7XHJcbiAgICBkb2N1bWVudC5yZW1vdmVFdmVudExpc3RlbmVyKFwiRE9NQ29udGVudExvYWRlZFwiLCB0aGlzLnRyYWNlKTtcclxuICAgIHRoaXMub2JzZXJ2ZXIuZGlzY29ubmVjdCgpO1xyXG4gICAgY2xlYXJUaW1lb3V0KHRoaXMudGltZW91dCk7XHJcbiAgfVxyXG59XHJcblxyXG5jbGFzcyBDb250ZW50RmlsdGVyaW5nXHJcbntcclxuICBjb25zdHJ1Y3RvcigpXHJcbiAge1xyXG4gICAgdGhpcy5zdHlsZXMgPSBuZXcgTWFwKCk7XHJcbiAgICB0aGlzLnRyYWNlciA9IG51bGw7XHJcbiAgICB0aGlzLmNzc1Byb3BlcnRpZXMgPSBudWxsO1xyXG4gICAgdGhpcy5lbGVtSGlkZUVtdWxhdGlvbiA9XHJcbiAgICAgIG5ldyBFbGVtSGlkZUVtdWxhdGlvbih0aGlzLmhpZGVFbGVtZW50cy5iaW5kKHRoaXMpKTtcclxuICB9XHJcblxyXG4gIGFkZFJ1bGVzSW5saW5lKHJ1bGVzLCBncm91cE5hbWUgPSBcInN0YW5kYXJkXCIsIGFwcGVuZE9ubHkgPSBmYWxzZSlcclxuICB7XHJcbiAgICBsZXQgc3R5bGUgPSB0aGlzLnN0eWxlcy5nZXQoZ3JvdXBOYW1lKTtcclxuXHJcbiAgICBpZiAoc3R5bGUgJiYgIWFwcGVuZE9ubHkpXHJcbiAgICB7XHJcbiAgICAgIHdoaWxlIChzdHlsZS5zaGVldC5jc3NSdWxlcy5sZW5ndGggPiAwKVxyXG4gICAgICAgIHN0eWxlLnNoZWV0LmRlbGV0ZVJ1bGUoMCk7XHJcbiAgICB9XHJcblxyXG4gICAgaWYgKHJ1bGVzLmxlbmd0aCA9PSAwKVxyXG4gICAgICByZXR1cm47XHJcblxyXG4gICAgaWYgKCFzdHlsZSlcclxuICAgIHtcclxuICAgICAgLy8gQ3JlYXRlIDxzdHlsZT4gZWxlbWVudCBsYXppbHksIG9ubHkgaWYgd2UgYWRkIHN0eWxlcy4gQWRkIGl0IHRvXHJcbiAgICAgIC8vIHRoZSA8aGVhZD4gb3IgPGh0bWw+IGVsZW1lbnQuIElmIHdlIGhhdmUgaW5qZWN0ZWQgYSBzdHlsZSBlbGVtZW50XHJcbiAgICAgIC8vIGJlZm9yZSB0aGF0IGhhcyBiZWVuIHJlbW92ZWQgKHRoZSBzaGVldCBwcm9wZXJ0eSBpcyBudWxsKSwgY3JlYXRlIGFcclxuICAgICAgLy8gbmV3IG9uZS5cclxuICAgICAgc3R5bGUgPSBkb2N1bWVudC5jcmVhdGVFbGVtZW50KFwic3R5bGVcIik7XHJcbiAgICAgIChkb2N1bWVudC5oZWFkIHx8IGRvY3VtZW50LmRvY3VtZW50RWxlbWVudCkuYXBwZW5kQ2hpbGQoc3R5bGUpO1xyXG5cclxuICAgICAgLy8gSXQgY2FuIGhhcHBlbiB0aGF0IHRoZSBmcmFtZSBhbHJlYWR5IG5hdmlnYXRlZCB0byBhIGRpZmZlcmVudFxyXG4gICAgICAvLyBkb2N1bWVudCB3aGlsZSB3ZSB3ZXJlIHdhaXRpbmcgZm9yIHRoZSBiYWNrZ3JvdW5kIHBhZ2UgdG8gcmVzcG9uZC5cclxuICAgICAgLy8gSW4gdGhhdCBjYXNlIHRoZSBzaGVldCBwcm9wZXJ0eSBtYXkgc3RheSBudWxsLCBhZnRlciBhZGRpbmcgdGhlXHJcbiAgICAgIC8vIDxzdHlsZT4gZWxlbWVudC5cclxuICAgICAgaWYgKCFzdHlsZS5zaGVldClcclxuICAgICAgICByZXR1cm47XHJcblxyXG4gICAgICB0aGlzLnN0eWxlcy5zZXQoZ3JvdXBOYW1lLCBzdHlsZSk7XHJcbiAgICB9XHJcblxyXG4gICAgZm9yIChsZXQgcnVsZSBvZiBydWxlcylcclxuICAgICAgc3R5bGUuc2hlZXQuaW5zZXJ0UnVsZShydWxlLCBzdHlsZS5zaGVldC5jc3NSdWxlcy5sZW5ndGgpO1xyXG4gIH1cclxuXHJcbiAgYXN5bmMgYWRkU2VsZWN0b3JzKHNlbGVjdG9ycywgZ3JvdXBOYW1lID0gXCJzdGFuZGFyZFwiLCBhcHBlbmRPbmx5ID0gZmFsc2UpXHJcbiAge1xyXG4gICAgbGV0IHJ1bGVzID0gYXdhaXQgYnJvd3Nlci5ydW50aW1lLnNlbmRNZXNzYWdlKHtcclxuICAgICAgdHlwZTogXCJjb250ZW50LmluamVjdFNlbGVjdG9yc1wiLFxyXG4gICAgICBzZWxlY3RvcnMsXHJcbiAgICAgIGdyb3VwTmFtZSxcclxuICAgICAgYXBwZW5kT25seVxyXG4gICAgfSk7XHJcbiAgICBpZiAocnVsZXMpXHJcbiAgICB7XHJcbiAgICAgIC8vIEluc2VydCB0aGUgcnVsZXMgaW5saW5lIGlmIHdlIGhhdmUgYmVlbiBpbnN0cnVjdGVkIGJ5IHRoZSBiYWNrZ3JvdW5kXHJcbiAgICAgIC8vIHBhZ2UgdG8gZG8gc28uIFRoaXMgaXMgcmFyZWx5IHRoZSBjYXNlLCBleGNlcHQgb24gcGxhdGZvcm1zIHRoYXQgZG9cclxuICAgICAgLy8gbm90IHN1cHBvcnQgdXNlciBzdHlsZXNoZWV0cyB2aWEgdGhlIGJyb3dzZXIudGFicy5pbnNlcnRDU1MgQVBJLCBpLmUuXHJcbiAgICAgIC8vIEZpcmVmb3ggPDUzIGFuZCBDaHJvbWUgPDY2LlxyXG4gICAgICAvLyBPbmNlIGFsbCBzdXBwb3J0ZWQgcGxhdGZvcm1zIGhhdmUgaW1wbGVtZW50ZWQgdGhpcyBBUEksIHdlIGNhbiByZW1vdmVcclxuICAgICAgLy8gdGhlIGNvZGUgYmVsb3cuIFNlZSBpc3N1ZSAjNTA5MC5cclxuICAgICAgLy8gUmVsYXRlZCBDaHJvbWUgYW5kIEZpcmVmb3ggaXNzdWVzOlxyXG4gICAgICAvLyBodHRwczovL2J1Z3MuY2hyb21pdW0ub3JnL3AvY2hyb21pdW0vaXNzdWVzL2RldGFpbD9pZD02MzIwMDlcclxuICAgICAgLy8gaHR0cHM6Ly9idWd6aWxsYS5tb3ppbGxhLm9yZy9zaG93X2J1Zy5jZ2k/aWQ9MTMxMDAyNlxyXG4gICAgICB0aGlzLmFkZFJ1bGVzSW5saW5lKHJ1bGVzLCBncm91cE5hbWUsIGFwcGVuZE9ubHkpO1xyXG4gICAgfVxyXG4gIH1cclxuXHJcbiAgaGlkZUVsZW1lbnRzKGVsZW1lbnRzLCBmaWx0ZXJzKVxyXG4gIHtcclxuICAgIGZvciAobGV0IGVsZW1lbnQgb2YgZWxlbWVudHMpXHJcbiAgICAgIGhpZGVFbGVtZW50KGVsZW1lbnQsIHRoaXMuY3NzUHJvcGVydGllcyk7XHJcblxyXG4gICAgaWYgKHRoaXMudHJhY2VyKVxyXG4gICAge1xyXG4gICAgICBicm93c2VyLnJ1bnRpbWUuc2VuZE1lc3NhZ2Uoe1xyXG4gICAgICAgIHR5cGU6IFwiaGl0TG9nZ2VyLnRyYWNlRWxlbUhpZGVcIixcclxuICAgICAgICBzZWxlY3RvcnM6IFtdLFxyXG4gICAgICAgIGZpbHRlcnNcclxuICAgICAgfSk7XHJcbiAgICB9XHJcbiAgfVxyXG5cclxuICBhc3luYyBhcHBseShmaWx0ZXJUeXBlcylcclxuICB7XHJcbiAgICBsZXQgcmVzcG9uc2UgPSBhd2FpdCBicm93c2VyLnJ1bnRpbWUuc2VuZE1lc3NhZ2Uoe1xyXG4gICAgICB0eXBlOiBcImNvbnRlbnQuYXBwbHlGaWx0ZXJzXCIsXHJcbiAgICAgIGZpbHRlclR5cGVzXHJcbiAgICB9KTtcclxuXHJcbiAgICBpZiAodGhpcy50cmFjZXIpXHJcbiAgICB7XHJcbiAgICAgIHRoaXMudHJhY2VyLmRpc2Nvbm5lY3QoKTtcclxuICAgICAgdGhpcy50cmFjZXIgPSBudWxsO1xyXG4gICAgfVxyXG5cclxuICAgIGlmIChyZXNwb25zZS5pbmxpbmUpXHJcbiAgICAgIHRoaXMuYWRkUnVsZXNJbmxpbmUocmVzcG9uc2UucnVsZXMpO1xyXG5cclxuICAgIGlmIChyZXNwb25zZS50cmFjZSlcclxuICAgIHtcclxuICAgICAgdGhpcy50cmFjZXIgPSBuZXcgRWxlbWVudEhpZGluZ1RyYWNlcihcclxuICAgICAgICByZXNwb25zZS5zZWxlY3RvcnMsXHJcbiAgICAgICAgcmVzcG9uc2UuZXhjZXB0aW9uc1xyXG4gICAgICApO1xyXG4gICAgfVxyXG5cclxuICAgIHRoaXMuY3NzUHJvcGVydGllcyA9IHJlc3BvbnNlLmNzc1Byb3BlcnRpZXM7XHJcbiAgICB0aGlzLmVsZW1IaWRlRW11bGF0aW9uLmFwcGx5KHJlc3BvbnNlLmVtdWxhdGVkUGF0dGVybnMpO1xyXG4gIH1cclxufVxyXG5cclxuaWYgKGRvY3VtZW50IGluc3RhbmNlb2YgSFRNTERvY3VtZW50KVxyXG57XHJcbiAgY2hlY2tTaXRla2V5KCk7XHJcblxyXG4gIGNvbnRlbnRGaWx0ZXJpbmcgPSBuZXcgQ29udGVudEZpbHRlcmluZygpO1xyXG4gIGNvbnRlbnRGaWx0ZXJpbmcuYXBwbHkoKTtcclxuXHJcbiAgc3RhcnRFbGVtZW50Q29sbGFwc2luZygpO1xyXG59XHJcbiIsIi8qXHJcbiAqIFRoaXMgZmlsZSBpcyBwYXJ0IG9mIEFkYmxvY2sgUGx1cyA8aHR0cHM6Ly9hZGJsb2NrcGx1cy5vcmcvPixcclxuICogQ29weXJpZ2h0IChDKSAyMDA2LXByZXNlbnQgZXllbyBHbWJIXHJcbiAqXHJcbiAqIEFkYmxvY2sgUGx1cyBpcyBmcmVlIHNvZnR3YXJlOiB5b3UgY2FuIHJlZGlzdHJpYnV0ZSBpdCBhbmQvb3IgbW9kaWZ5XHJcbiAqIGl0IHVuZGVyIHRoZSB0ZXJtcyBvZiB0aGUgR05VIEdlbmVyYWwgUHVibGljIExpY2Vuc2UgdmVyc2lvbiAzIGFzXHJcbiAqIHB1Ymxpc2hlZCBieSB0aGUgRnJlZSBTb2Z0d2FyZSBGb3VuZGF0aW9uLlxyXG4gKlxyXG4gKiBBZGJsb2NrIFBsdXMgaXMgZGlzdHJpYnV0ZWQgaW4gdGhlIGhvcGUgdGhhdCBpdCB3aWxsIGJlIHVzZWZ1bCxcclxuICogYnV0IFdJVEhPVVQgQU5ZIFdBUlJBTlRZOyB3aXRob3V0IGV2ZW4gdGhlIGltcGxpZWQgd2FycmFudHkgb2ZcclxuICogTUVSQ0hBTlRBQklMSVRZIG9yIEZJVE5FU1MgRk9SIEEgUEFSVElDVUxBUiBQVVJQT1NFLiAgU2VlIHRoZVxyXG4gKiBHTlUgR2VuZXJhbCBQdWJsaWMgTGljZW5zZSBmb3IgbW9yZSBkZXRhaWxzLlxyXG4gKlxyXG4gKiBZb3Ugc2hvdWxkIGhhdmUgcmVjZWl2ZWQgYSBjb3B5IG9mIHRoZSBHTlUgR2VuZXJhbCBQdWJsaWMgTGljZW5zZVxyXG4gKiBhbG9uZyB3aXRoIEFkYmxvY2sgUGx1cy4gIElmIG5vdCwgc2VlIDxodHRwOi8vd3d3LmdudS5vcmcvbGljZW5zZXMvPi5cclxuICovXHJcblxyXG5sZXQgcmFuZG9tRXZlbnROYW1lID0gXCJhYnAtcmVxdWVzdC1cIiArIE1hdGgucmFuZG9tKCkudG9TdHJpbmcoMzYpLnN1YnN0cigyKTtcclxuXHJcbi8vIFByb3h5IFwic2hvdWxkIHdlIGJsb2NrP1wiIG1lc3NhZ2VzIGZyb20gY2hlY2tSZXF1ZXN0IGluc2lkZSB0aGUgaW5qZWN0ZWRcclxuLy8gY29kZSB0byB0aGUgYmFja2dyb3VuZCBwYWdlIGFuZCBiYWNrIGFnYWluLlxyXG5kb2N1bWVudC5hZGRFdmVudExpc3RlbmVyKHJhbmRvbUV2ZW50TmFtZSwgYXN5bmMgZXZlbnQgPT5cclxue1xyXG4gIGxldCB7dXJsfSA9IGV2ZW50LmRldGFpbDtcclxuXHJcbiAgbGV0IGJsb2NrID0gYXdhaXQgYnJvd3Nlci5ydW50aW1lLnNlbmRNZXNzYWdlKHtcclxuICAgIHR5cGU6IFwicmVxdWVzdC5ibG9ja2VkQnlSVENXcmFwcGVyXCIsXHJcbiAgICB1cmxcclxuICB9KTtcclxuICBkb2N1bWVudC5kaXNwYXRjaEV2ZW50KG5ldyBDdXN0b21FdmVudChcclxuICAgIHJhbmRvbUV2ZW50TmFtZSArIFwiLVwiICsgdXJsLCB7ZGV0YWlsOiBibG9ja31cclxuICApKTtcclxufSk7XHJcblxyXG5mdW5jdGlvbiBpbmplY3RlZChldmVudE5hbWUsIGluamVjdGVkSW50b0NvbnRlbnRXaW5kb3cpXHJcbntcclxuICBsZXQgY2hlY2tSZXF1ZXN0O1xyXG5cclxuICAvKlxyXG4gICAqIEZyYW1lIGNvbnRleHQgd3JhcHBlclxyXG4gICAqXHJcbiAgICogRm9yIHNvbWUgZWRnZS1jYXNlcyBDaHJvbWUgd2lsbCBub3QgcnVuIGNvbnRlbnQgc2NyaXB0cyBpbnNpZGUgb2YgZnJhbWVzLlxyXG4gICAqIFdlYnNpdGUgaGF2ZSBzdGFydGVkIHRvIGFidXNlIHRoaXMgZmFjdCB0byBhY2Nlc3MgdW53cmFwcGVkIEFQSXMgdmlhIGFcclxuICAgKiBmcmFtZSdzIGNvbnRlbnRXaW5kb3cgKCM0NTg2LCA1MjA3KS4gVGhlcmVmb3JlIHVudGlsIENocm9tZSBydW5zIGNvbnRlbnRcclxuICAgKiBzY3JpcHRzIGNvbnNpc3RlbnRseSBmb3IgYWxsIGZyYW1lcyB3ZSBtdXN0IHRha2UgY2FyZSB0byAocmUpaW5qZWN0IG91clxyXG4gICAqIHdyYXBwZXJzIHdoZW4gdGhlIGNvbnRlbnRXaW5kb3cgaXMgYWNjZXNzZWQuXHJcbiAgICovXHJcbiAgbGV0IGluamVjdGVkVG9TdHJpbmcgPSBGdW5jdGlvbi5wcm90b3R5cGUudG9TdHJpbmcuYmluZChpbmplY3RlZCk7XHJcbiAgbGV0IGluamVjdGVkRnJhbWVzID0gbmV3IFdlYWtTZXQoKTtcclxuICBsZXQgaW5qZWN0ZWRGcmFtZXNBZGQgPSBXZWFrU2V0LnByb3RvdHlwZS5hZGQuYmluZChpbmplY3RlZEZyYW1lcyk7XHJcbiAgbGV0IGluamVjdGVkRnJhbWVzSGFzID0gV2Vha1NldC5wcm90b3R5cGUuaGFzLmJpbmQoaW5qZWN0ZWRGcmFtZXMpO1xyXG5cclxuICBmdW5jdGlvbiBpbmplY3RJbnRvQ29udGVudFdpbmRvdyhjb250ZW50V2luZG93KVxyXG4gIHtcclxuICAgIGlmIChjb250ZW50V2luZG93ICYmICFpbmplY3RlZEZyYW1lc0hhcyhjb250ZW50V2luZG93KSlcclxuICAgIHtcclxuICAgICAgaW5qZWN0ZWRGcmFtZXNBZGQoY29udGVudFdpbmRvdyk7XHJcbiAgICAgIHRyeVxyXG4gICAgICB7XHJcbiAgICAgICAgY29udGVudFdpbmRvd1tldmVudE5hbWVdID0gY2hlY2tSZXF1ZXN0O1xyXG4gICAgICAgIGNvbnRlbnRXaW5kb3cuZXZhbChcclxuICAgICAgICAgIFwiKFwiICsgaW5qZWN0ZWRUb1N0cmluZygpICsgXCIpKCdcIiArIGV2ZW50TmFtZSArIFwiJywgdHJ1ZSk7XCJcclxuICAgICAgICApO1xyXG4gICAgICAgIGRlbGV0ZSBjb250ZW50V2luZG93W2V2ZW50TmFtZV07XHJcbiAgICAgIH1cclxuICAgICAgY2F0Y2ggKGUpIHt9XHJcbiAgICB9XHJcbiAgfVxyXG5cclxuICBmb3IgKGxldCBlbGVtZW50IG9mIFtIVE1MRnJhbWVFbGVtZW50LCBIVE1MSUZyYW1lRWxlbWVudCwgSFRNTE9iamVjdEVsZW1lbnRdKVxyXG4gIHtcclxuICAgIGxldCBjb250ZW50RG9jdW1lbnREZXNjID0gT2JqZWN0LmdldE93blByb3BlcnR5RGVzY3JpcHRvcihcclxuICAgICAgZWxlbWVudC5wcm90b3R5cGUsIFwiY29udGVudERvY3VtZW50XCJcclxuICAgICk7XHJcbiAgICBsZXQgY29udGVudFdpbmRvd0Rlc2MgPSBPYmplY3QuZ2V0T3duUHJvcGVydHlEZXNjcmlwdG9yKFxyXG4gICAgICBlbGVtZW50LnByb3RvdHlwZSwgXCJjb250ZW50V2luZG93XCJcclxuICAgICk7XHJcblxyXG4gICAgLy8gQXBwYXJlbnRseSBpbiBIVE1MT2JqZWN0RWxlbWVudC5wcm90b3R5cGUuY29udGVudFdpbmRvdyBkb2VzIG5vdCBleGlzdFxyXG4gICAgLy8gaW4gb2xkZXIgdmVyc2lvbnMgb2YgQ2hyb21lIHN1Y2ggYXMgNTEuXHJcbiAgICBpZiAoIWNvbnRlbnRXaW5kb3dEZXNjKVxyXG4gICAgICBjb250aW51ZTtcclxuXHJcbiAgICBsZXQgZ2V0Q29udGVudERvY3VtZW50ID0gRnVuY3Rpb24ucHJvdG90eXBlLmNhbGwuYmluZChcclxuICAgICAgY29udGVudERvY3VtZW50RGVzYy5nZXRcclxuICAgICk7XHJcbiAgICBsZXQgZ2V0Q29udGVudFdpbmRvdyA9IEZ1bmN0aW9uLnByb3RvdHlwZS5jYWxsLmJpbmQoXHJcbiAgICAgIGNvbnRlbnRXaW5kb3dEZXNjLmdldFxyXG4gICAgKTtcclxuXHJcbiAgICBjb250ZW50V2luZG93RGVzYy5nZXQgPSBmdW5jdGlvbigpXHJcbiAgICB7XHJcbiAgICAgIGxldCBjb250ZW50V2luZG93ID0gZ2V0Q29udGVudFdpbmRvdyh0aGlzKTtcclxuICAgICAgaW5qZWN0SW50b0NvbnRlbnRXaW5kb3coY29udGVudFdpbmRvdyk7XHJcbiAgICAgIHJldHVybiBjb250ZW50V2luZG93O1xyXG4gICAgfTtcclxuICAgIGNvbnRlbnREb2N1bWVudERlc2MuZ2V0ID0gZnVuY3Rpb24oKVxyXG4gICAge1xyXG4gICAgICBpbmplY3RJbnRvQ29udGVudFdpbmRvdyhnZXRDb250ZW50V2luZG93KHRoaXMpKTtcclxuICAgICAgcmV0dXJuIGdldENvbnRlbnREb2N1bWVudCh0aGlzKTtcclxuICAgIH07XHJcbiAgICBPYmplY3QuZGVmaW5lUHJvcGVydHkoZWxlbWVudC5wcm90b3R5cGUsIFwiY29udGVudFdpbmRvd1wiLFxyXG4gICAgICAgICAgICAgICAgICAgICAgICAgIGNvbnRlbnRXaW5kb3dEZXNjKTtcclxuICAgIE9iamVjdC5kZWZpbmVQcm9wZXJ0eShlbGVtZW50LnByb3RvdHlwZSwgXCJjb250ZW50RG9jdW1lbnRcIixcclxuICAgICAgICAgICAgICAgICAgICAgICAgICBjb250ZW50RG9jdW1lbnREZXNjKTtcclxuICB9XHJcblxyXG4gIC8qXHJcbiAgICogUlRDUGVlckNvbm5lY3Rpb24gd3JhcHBlclxyXG4gICAqXHJcbiAgICogVGhlIHdlYlJlcXVlc3QgQVBJIGluIENocm9tZSBkb2VzIG5vdCB5ZXQgYWxsb3cgdGhlIGJsb2NraW5nIG9mXHJcbiAgICogV2ViUlRDIGNvbm5lY3Rpb25zLlxyXG4gICAqIFNlZSBodHRwczovL2J1Z3MuY2hyb21pdW0ub3JnL3AvY2hyb21pdW0vaXNzdWVzL2RldGFpbD9pZD03MDc2ODNcclxuICAgKi9cclxuICBsZXQgUmVhbEN1c3RvbUV2ZW50ID0gd2luZG93LkN1c3RvbUV2ZW50O1xyXG5cclxuICAvLyBJZiB3ZSd2ZSBiZWVuIGluamVjdGVkIGludG8gYSBmcmFtZSB2aWEgY29udGVudFdpbmRvdyB0aGVuIHdlIGNhbiBzaW1wbHlcclxuICAvLyBncmFiIHRoZSBjb3B5IG9mIGNoZWNrUmVxdWVzdCBsZWZ0IGZvciB1cyBieSB0aGUgcGFyZW50IGRvY3VtZW50LiBPdGhlcndpc2VcclxuICAvLyB3ZSBuZWVkIHRvIHNldCBpdCB1cCBub3csIGFsb25nIHdpdGggdGhlIGV2ZW50IGhhbmRsaW5nIGZ1bmN0aW9ucy5cclxuICBpZiAoaW5qZWN0ZWRJbnRvQ29udGVudFdpbmRvdylcclxuICB7XHJcbiAgICBjaGVja1JlcXVlc3QgPSB3aW5kb3dbZXZlbnROYW1lXTtcclxuICB9XHJcbiAgZWxzZVxyXG4gIHtcclxuICAgIGxldCBhZGRFdmVudExpc3RlbmVyID0gZG9jdW1lbnQuYWRkRXZlbnRMaXN0ZW5lci5iaW5kKGRvY3VtZW50KTtcclxuICAgIGxldCBkaXNwYXRjaEV2ZW50ID0gZG9jdW1lbnQuZGlzcGF0Y2hFdmVudC5iaW5kKGRvY3VtZW50KTtcclxuICAgIGxldCByZW1vdmVFdmVudExpc3RlbmVyID0gZG9jdW1lbnQucmVtb3ZlRXZlbnRMaXN0ZW5lci5iaW5kKGRvY3VtZW50KTtcclxuICAgIGNoZWNrUmVxdWVzdCA9ICh1cmwsIGNhbGxiYWNrKSA9PlxyXG4gICAge1xyXG4gICAgICBsZXQgaW5jb21pbmdFdmVudE5hbWUgPSBldmVudE5hbWUgKyBcIi1cIiArIHVybDtcclxuXHJcbiAgICAgIGZ1bmN0aW9uIGxpc3RlbmVyKGV2ZW50KVxyXG4gICAgICB7XHJcbiAgICAgICAgY2FsbGJhY2soZXZlbnQuZGV0YWlsKTtcclxuICAgICAgICByZW1vdmVFdmVudExpc3RlbmVyKGluY29taW5nRXZlbnROYW1lLCBsaXN0ZW5lcik7XHJcbiAgICAgIH1cclxuICAgICAgYWRkRXZlbnRMaXN0ZW5lcihpbmNvbWluZ0V2ZW50TmFtZSwgbGlzdGVuZXIpO1xyXG5cclxuICAgICAgZGlzcGF0Y2hFdmVudChuZXcgUmVhbEN1c3RvbUV2ZW50KGV2ZW50TmFtZSwge2RldGFpbDoge3VybH19KSk7XHJcbiAgICB9O1xyXG4gIH1cclxuXHJcbiAgLy8gT25seSB0byBiZSBjYWxsZWQgYmVmb3JlIHRoZSBwYWdlJ3MgY29kZSwgbm90IGhhcmRlbmVkLlxyXG4gIGZ1bmN0aW9uIGNvcHlQcm9wZXJ0aWVzKHNyYywgZGVzdCwgcHJvcGVydGllcylcclxuICB7XHJcbiAgICBmb3IgKGxldCBuYW1lIG9mIHByb3BlcnRpZXMpXHJcbiAgICB7XHJcbiAgICAgIGlmIChPYmplY3QucHJvdG90eXBlLmhhc093blByb3BlcnR5LmNhbGwoc3JjLCBuYW1lKSlcclxuICAgICAge1xyXG4gICAgICAgIE9iamVjdC5kZWZpbmVQcm9wZXJ0eShkZXN0LCBuYW1lLFxyXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICBPYmplY3QuZ2V0T3duUHJvcGVydHlEZXNjcmlwdG9yKHNyYywgbmFtZSkpO1xyXG4gICAgICB9XHJcbiAgICB9XHJcbiAgfVxyXG5cclxuICBsZXQgUmVhbFJUQ1BlZXJDb25uZWN0aW9uID0gd2luZG93LlJUQ1BlZXJDb25uZWN0aW9uIHx8XHJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHdpbmRvdy53ZWJraXRSVENQZWVyQ29ubmVjdGlvbjtcclxuXHJcbiAgLy8gRmlyZWZveCBoYXMgdGhlIG9wdGlvbiAobWVkaWEucGVlcmNvbm5lY3Rpb24uZW5hYmxlZCkgdG8gZGlzYWJsZSBXZWJSVENcclxuICAvLyBpbiB3aGljaCBjYXNlIFJlYWxSVENQZWVyQ29ubmVjdGlvbiBpcyB1bmRlZmluZWQuXHJcbiAgaWYgKHR5cGVvZiBSZWFsUlRDUGVlckNvbm5lY3Rpb24gIT0gXCJ1bmRlZmluZWRcIilcclxuICB7XHJcbiAgICBsZXQgY2xvc2VSVENQZWVyQ29ubmVjdGlvbiA9IEZ1bmN0aW9uLnByb3RvdHlwZS5jYWxsLmJpbmQoXHJcbiAgICAgIFJlYWxSVENQZWVyQ29ubmVjdGlvbi5wcm90b3R5cGUuY2xvc2VcclxuICAgICk7XHJcbiAgICBsZXQgUmVhbEFycmF5ID0gQXJyYXk7XHJcbiAgICBsZXQgUmVhbFN0cmluZyA9IFN0cmluZztcclxuICAgIGxldCB7Y3JlYXRlOiBjcmVhdGVPYmplY3QsIGRlZmluZVByb3BlcnR5fSA9IE9iamVjdDtcclxuXHJcbiAgICBsZXQgbm9ybWFsaXplVXJsID0gdXJsID0+XHJcbiAgICB7XHJcbiAgICAgIGlmICh0eXBlb2YgdXJsICE9IFwidW5kZWZpbmVkXCIpXHJcbiAgICAgICAgcmV0dXJuIFJlYWxTdHJpbmcodXJsKTtcclxuICAgIH07XHJcblxyXG4gICAgbGV0IHNhZmVDb3B5QXJyYXkgPSAob3JpZ2luYWxBcnJheSwgdHJhbnNmb3JtKSA9PlxyXG4gICAge1xyXG4gICAgICBpZiAob3JpZ2luYWxBcnJheSA9PSBudWxsIHx8IHR5cGVvZiBvcmlnaW5hbEFycmF5ICE9IFwib2JqZWN0XCIpXHJcbiAgICAgICAgcmV0dXJuIG9yaWdpbmFsQXJyYXk7XHJcblxyXG4gICAgICBsZXQgc2FmZUFycmF5ID0gUmVhbEFycmF5KG9yaWdpbmFsQXJyYXkubGVuZ3RoKTtcclxuICAgICAgZm9yIChsZXQgaSA9IDA7IGkgPCBzYWZlQXJyYXkubGVuZ3RoOyBpKyspXHJcbiAgICAgIHtcclxuICAgICAgICBkZWZpbmVQcm9wZXJ0eShzYWZlQXJyYXksIGksIHtcclxuICAgICAgICAgIGNvbmZpZ3VyYWJsZTogZmFsc2UsIGVudW1lcmFibGU6IGZhbHNlLCB3cml0YWJsZTogZmFsc2UsXHJcbiAgICAgICAgICB2YWx1ZTogdHJhbnNmb3JtKG9yaWdpbmFsQXJyYXlbaV0pXHJcbiAgICAgICAgfSk7XHJcbiAgICAgIH1cclxuICAgICAgZGVmaW5lUHJvcGVydHkoc2FmZUFycmF5LCBcImxlbmd0aFwiLCB7XHJcbiAgICAgICAgY29uZmlndXJhYmxlOiBmYWxzZSwgZW51bWVyYWJsZTogZmFsc2UsIHdyaXRhYmxlOiBmYWxzZSxcclxuICAgICAgICB2YWx1ZTogc2FmZUFycmF5Lmxlbmd0aFxyXG4gICAgICB9KTtcclxuICAgICAgcmV0dXJuIHNhZmVBcnJheTtcclxuICAgIH07XHJcblxyXG4gICAgLy8gSXQgd291bGQgYmUgbXVjaCBlYXNpZXIgdG8gdXNlIHRoZSAuZ2V0Q29uZmlndXJhdGlvbiBtZXRob2QgdG8gb2J0YWluXHJcbiAgICAvLyB0aGUgbm9ybWFsaXplZCBhbmQgc2FmZSBjb25maWd1cmF0aW9uIGZyb20gdGhlIFJUQ1BlZXJDb25uZWN0aW9uXHJcbiAgICAvLyBpbnN0YW5jZS4gVW5mb3J0dW5hdGVseSBpdHMgbm90IGltcGxlbWVudGVkIGFzIG9mIENocm9tZSB1bnN0YWJsZSA1OS5cclxuICAgIC8vIFNlZSBodHRwczovL3d3dy5jaHJvbWVzdGF0dXMuY29tL2ZlYXR1cmUvNTI3MTM1NTMwNjAxNjc2OFxyXG4gICAgbGV0IHByb3RlY3RDb25maWd1cmF0aW9uID0gY29uZmlndXJhdGlvbiA9PlxyXG4gICAge1xyXG4gICAgICBpZiAoY29uZmlndXJhdGlvbiA9PSBudWxsIHx8IHR5cGVvZiBjb25maWd1cmF0aW9uICE9IFwib2JqZWN0XCIpXHJcbiAgICAgICAgcmV0dXJuIGNvbmZpZ3VyYXRpb247XHJcblxyXG4gICAgICBsZXQgaWNlU2VydmVycyA9IHNhZmVDb3B5QXJyYXkoXHJcbiAgICAgICAgY29uZmlndXJhdGlvbi5pY2VTZXJ2ZXJzLFxyXG4gICAgICAgIGljZVNlcnZlciA9PlxyXG4gICAgICAgIHtcclxuICAgICAgICAgIGxldCB7dXJsLCB1cmxzfSA9IGljZVNlcnZlcjtcclxuXHJcbiAgICAgICAgICAvLyBSVENQZWVyQ29ubmVjdGlvbiBkb2Vzbid0IGl0ZXJhdGUgdGhyb3VnaCBwc2V1ZG8gQXJyYXlzIG9mIHVybHMuXHJcbiAgICAgICAgICBpZiAodHlwZW9mIHVybHMgIT0gXCJ1bmRlZmluZWRcIiAmJiAhKHVybHMgaW5zdGFuY2VvZiBSZWFsQXJyYXkpKVxyXG4gICAgICAgICAgICB1cmxzID0gW3VybHNdO1xyXG5cclxuICAgICAgICAgIHJldHVybiBjcmVhdGVPYmplY3QoaWNlU2VydmVyLCB7XHJcbiAgICAgICAgICAgIHVybDoge1xyXG4gICAgICAgICAgICAgIGNvbmZpZ3VyYWJsZTogZmFsc2UsIGVudW1lcmFibGU6IGZhbHNlLCB3cml0YWJsZTogZmFsc2UsXHJcbiAgICAgICAgICAgICAgdmFsdWU6IG5vcm1hbGl6ZVVybCh1cmwpXHJcbiAgICAgICAgICAgIH0sXHJcbiAgICAgICAgICAgIHVybHM6IHtcclxuICAgICAgICAgICAgICBjb25maWd1cmFibGU6IGZhbHNlLCBlbnVtZXJhYmxlOiBmYWxzZSwgd3JpdGFibGU6IGZhbHNlLFxyXG4gICAgICAgICAgICAgIHZhbHVlOiBzYWZlQ29weUFycmF5KHVybHMsIG5vcm1hbGl6ZVVybClcclxuICAgICAgICAgICAgfVxyXG4gICAgICAgICAgfSk7XHJcbiAgICAgICAgfVxyXG4gICAgICApO1xyXG5cclxuICAgICAgcmV0dXJuIGNyZWF0ZU9iamVjdChjb25maWd1cmF0aW9uLCB7XHJcbiAgICAgICAgaWNlU2VydmVyczoge1xyXG4gICAgICAgICAgY29uZmlndXJhYmxlOiBmYWxzZSwgZW51bWVyYWJsZTogZmFsc2UsIHdyaXRhYmxlOiBmYWxzZSxcclxuICAgICAgICAgIHZhbHVlOiBpY2VTZXJ2ZXJzXHJcbiAgICAgICAgfVxyXG4gICAgICB9KTtcclxuICAgIH07XHJcblxyXG4gICAgbGV0IGNoZWNrVXJsID0gKHBlZXJjb25uZWN0aW9uLCB1cmwpID0+XHJcbiAgICB7XHJcbiAgICAgIGNoZWNrUmVxdWVzdCh1cmwsIGJsb2NrZWQgPT5cclxuICAgICAge1xyXG4gICAgICAgIGlmIChibG9ja2VkKVxyXG4gICAgICAgIHtcclxuICAgICAgICAgIC8vIENhbGxpbmcgLmNsb3NlKCkgdGhyb3dzIGlmIGFscmVhZHkgY2xvc2VkLlxyXG4gICAgICAgICAgdHJ5XHJcbiAgICAgICAgICB7XHJcbiAgICAgICAgICAgIGNsb3NlUlRDUGVlckNvbm5lY3Rpb24ocGVlcmNvbm5lY3Rpb24pO1xyXG4gICAgICAgICAgfVxyXG4gICAgICAgICAgY2F0Y2ggKGUpIHt9XHJcbiAgICAgICAgfVxyXG4gICAgICB9KTtcclxuICAgIH07XHJcblxyXG4gICAgbGV0IGNoZWNrQ29uZmlndXJhdGlvbiA9IChwZWVyY29ubmVjdGlvbiwgY29uZmlndXJhdGlvbikgPT5cclxuICAgIHtcclxuICAgICAgaWYgKGNvbmZpZ3VyYXRpb24gJiYgY29uZmlndXJhdGlvbi5pY2VTZXJ2ZXJzKVxyXG4gICAgICB7XHJcbiAgICAgICAgZm9yIChsZXQgaSA9IDA7IGkgPCBjb25maWd1cmF0aW9uLmljZVNlcnZlcnMubGVuZ3RoOyBpKyspXHJcbiAgICAgICAge1xyXG4gICAgICAgICAgbGV0IGljZVNlcnZlciA9IGNvbmZpZ3VyYXRpb24uaWNlU2VydmVyc1tpXTtcclxuICAgICAgICAgIGlmIChpY2VTZXJ2ZXIpXHJcbiAgICAgICAgICB7XHJcbiAgICAgICAgICAgIGlmIChpY2VTZXJ2ZXIudXJsKVxyXG4gICAgICAgICAgICAgIGNoZWNrVXJsKHBlZXJjb25uZWN0aW9uLCBpY2VTZXJ2ZXIudXJsKTtcclxuXHJcbiAgICAgICAgICAgIGlmIChpY2VTZXJ2ZXIudXJscylcclxuICAgICAgICAgICAge1xyXG4gICAgICAgICAgICAgIGZvciAobGV0IGogPSAwOyBqIDwgaWNlU2VydmVyLnVybHMubGVuZ3RoOyBqKyspXHJcbiAgICAgICAgICAgICAgICBjaGVja1VybChwZWVyY29ubmVjdGlvbiwgaWNlU2VydmVyLnVybHNbal0pO1xyXG4gICAgICAgICAgICB9XHJcbiAgICAgICAgICB9XHJcbiAgICAgICAgfVxyXG4gICAgICB9XHJcbiAgICB9O1xyXG5cclxuICAgIC8vIENocm9tZSB1bnN0YWJsZSAodGVzdGVkIHdpdGggNTkpIGhhcyBhbHJlYWR5IGltcGxlbWVudGVkXHJcbiAgICAvLyBzZXRDb25maWd1cmF0aW9uLCBzbyB3ZSBuZWVkIHRvIHdyYXAgdGhhdCBpZiBpdCBleGlzdHMgdG9vLlxyXG4gICAgLy8gaHR0cHM6Ly93d3cuY2hyb21lc3RhdHVzLmNvbS9mZWF0dXJlLzU1OTYxOTM3NDg5NDI4NDhcclxuICAgIGlmIChSZWFsUlRDUGVlckNvbm5lY3Rpb24ucHJvdG90eXBlLnNldENvbmZpZ3VyYXRpb24pXHJcbiAgICB7XHJcbiAgICAgIGxldCByZWFsU2V0Q29uZmlndXJhdGlvbiA9IEZ1bmN0aW9uLnByb3RvdHlwZS5jYWxsLmJpbmQoXHJcbiAgICAgICAgUmVhbFJUQ1BlZXJDb25uZWN0aW9uLnByb3RvdHlwZS5zZXRDb25maWd1cmF0aW9uXHJcbiAgICAgICk7XHJcblxyXG4gICAgICBSZWFsUlRDUGVlckNvbm5lY3Rpb24ucHJvdG90eXBlLnNldENvbmZpZ3VyYXRpb24gPSBmdW5jdGlvbihjb25maWd1cmF0aW9uKVxyXG4gICAgICB7XHJcbiAgICAgICAgY29uZmlndXJhdGlvbiA9IHByb3RlY3RDb25maWd1cmF0aW9uKGNvbmZpZ3VyYXRpb24pO1xyXG5cclxuICAgICAgICAvLyBDYWxsIHRoZSByZWFsIG1ldGhvZCBmaXJzdCwgc28gdGhhdCB2YWxpZGF0ZXMgdGhlIGNvbmZpZ3VyYXRpb24gZm9yXHJcbiAgICAgICAgLy8gdXMuIEFsc28gd2UgbWlnaHQgYXMgd2VsbCBzaW5jZSBjaGVja1JlcXVlc3QgaXMgYXN5bmNocm9ub3VzIGFueXdheS5cclxuICAgICAgICByZWFsU2V0Q29uZmlndXJhdGlvbih0aGlzLCBjb25maWd1cmF0aW9uKTtcclxuICAgICAgICBjaGVja0NvbmZpZ3VyYXRpb24odGhpcywgY29uZmlndXJhdGlvbik7XHJcbiAgICAgIH07XHJcbiAgICB9XHJcblxyXG4gICAgbGV0IFdyYXBwZWRSVENQZWVyQ29ubmVjdGlvbiA9IGZ1bmN0aW9uKC4uLmFyZ3MpXHJcbiAgICB7XHJcbiAgICAgIGlmICghKHRoaXMgaW5zdGFuY2VvZiBXcmFwcGVkUlRDUGVlckNvbm5lY3Rpb24pKVxyXG4gICAgICAgIHJldHVybiBSZWFsUlRDUGVlckNvbm5lY3Rpb24oKTtcclxuXHJcbiAgICAgIGxldCBjb25maWd1cmF0aW9uID0gcHJvdGVjdENvbmZpZ3VyYXRpb24oYXJnc1swXSk7XHJcblxyXG4gICAgICAvLyBTaW5jZSB0aGUgb2xkIHdlYmtpdFJUQ1BlZXJDb25uZWN0aW9uIGNvbnN0cnVjdG9yIHRha2VzIGFuIG9wdGlvbmFsXHJcbiAgICAgIC8vIHNlY29uZCBhcmd1bWVudCB3ZSBuZWVkIHRvIHRha2UgY2FyZSB0byBwYXNzIHRoYXQgdGhyb3VnaC4gTmVjZXNzYXJ5XHJcbiAgICAgIC8vIGZvciBvbGRlciB2ZXJzaW9ucyBvZiBDaHJvbWUgc3VjaCBhcyA1MS5cclxuICAgICAgbGV0IGNvbnN0cmFpbnRzO1xyXG4gICAgICBpZiAoYXJncy5sZW5ndGggPiAxKVxyXG4gICAgICAgIGNvbnN0cmFpbnRzID0gYXJnc1sxXTtcclxuXHJcbiAgICAgIGxldCBwZWVyY29ubmVjdGlvbiA9IG5ldyBSZWFsUlRDUGVlckNvbm5lY3Rpb24oY29uZmlndXJhdGlvbixcclxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBjb25zdHJhaW50cyk7XHJcbiAgICAgIGNoZWNrQ29uZmlndXJhdGlvbihwZWVyY29ubmVjdGlvbiwgY29uZmlndXJhdGlvbik7XHJcbiAgICAgIHJldHVybiBwZWVyY29ubmVjdGlvbjtcclxuICAgIH07XHJcblxyXG4gICAgV3JhcHBlZFJUQ1BlZXJDb25uZWN0aW9uLnByb3RvdHlwZSA9IFJlYWxSVENQZWVyQ29ubmVjdGlvbi5wcm90b3R5cGU7XHJcblxyXG4gICAgbGV0IGJvdW5kV3JhcHBlZFJUQ1BlZXJDb25uZWN0aW9uID0gV3JhcHBlZFJUQ1BlZXJDb25uZWN0aW9uLmJpbmQoKTtcclxuICAgIGNvcHlQcm9wZXJ0aWVzKFJlYWxSVENQZWVyQ29ubmVjdGlvbiwgYm91bmRXcmFwcGVkUlRDUGVlckNvbm5lY3Rpb24sXHJcbiAgICAgICAgICAgICAgICAgICBbXCJnZW5lcmF0ZUNlcnRpZmljYXRlXCIsIFwibmFtZVwiLCBcInByb3RvdHlwZVwiXSk7XHJcbiAgICBSZWFsUlRDUGVlckNvbm5lY3Rpb24ucHJvdG90eXBlLmNvbnN0cnVjdG9yID0gYm91bmRXcmFwcGVkUlRDUGVlckNvbm5lY3Rpb247XHJcblxyXG4gICAgaWYgKFwiUlRDUGVlckNvbm5lY3Rpb25cIiBpbiB3aW5kb3cpXHJcbiAgICAgIHdpbmRvdy5SVENQZWVyQ29ubmVjdGlvbiA9IGJvdW5kV3JhcHBlZFJUQ1BlZXJDb25uZWN0aW9uO1xyXG4gICAgaWYgKFwid2Via2l0UlRDUGVlckNvbm5lY3Rpb25cIiBpbiB3aW5kb3cpXHJcbiAgICAgIHdpbmRvdy53ZWJraXRSVENQZWVyQ29ubmVjdGlvbiA9IGJvdW5kV3JhcHBlZFJUQ1BlZXJDb25uZWN0aW9uO1xyXG4gIH1cclxufVxyXG5cclxuaWYgKGRvY3VtZW50IGluc3RhbmNlb2YgSFRNTERvY3VtZW50KVxyXG57XHJcbiAgbGV0IHNhbmRib3ggPSB3aW5kb3cuZnJhbWVFbGVtZW50ICYmXHJcbiAgICAgICAgICAgICAgICB3aW5kb3cuZnJhbWVFbGVtZW50LmdldEF0dHJpYnV0ZShcInNhbmRib3hcIik7XHJcblxyXG4gIGlmICh0eXBlb2Ygc2FuZGJveCAhPSBcInN0cmluZ1wiIHx8IC8oXnxcXHMpYWxsb3ctc2NyaXB0cyhcXHN8JCkvaS50ZXN0KHNhbmRib3gpKVxyXG4gIHtcclxuICAgIGxldCBzY3JpcHQgPSBkb2N1bWVudC5jcmVhdGVFbGVtZW50KFwic2NyaXB0XCIpO1xyXG4gICAgbGV0IGNvZGUgPSBcIihcIiArIGluamVjdGVkICsgXCIpKCdcIiArIHJhbmRvbUV2ZW50TmFtZSArIFwiJyk7XCI7XHJcblxyXG4gICAgc2NyaXB0LnR5cGUgPSBcImFwcGxpY2F0aW9uL2phdmFzY3JpcHRcIjtcclxuICAgIHNjcmlwdC5hc3luYyA9IGZhbHNlO1xyXG5cclxuICAgIC8vIEZpcmVmb3ggNTggb25seSBieXBhc3NlcyBzaXRlIENTUHMgd2hlbiBhc3NpZ25pbmcgdG8gJ3NyYycsXHJcbiAgICAvLyB3aGlsZSBDaHJvbWUgNjcgb25seSBieXBhc3Mgc2l0ZSBDU1BzIHdoZW4gdXNpbmcgJ3RleHRDb250ZW50Jy5cclxuICAgIGlmIChicm93c2VyLnJ1bnRpbWUuZ2V0VVJMKFwiXCIpLnN0YXJ0c1dpdGgoXCJtb3otZXh0ZW5zaW9uOi8vXCIpKVxyXG4gICAge1xyXG4gICAgICBsZXQgdXJsID0gVVJMLmNyZWF0ZU9iamVjdFVSTChuZXcgQmxvYihbY29kZV0pKTtcclxuICAgICAgc2NyaXB0LnNyYyA9IHVybDtcclxuICAgICAgZG9jdW1lbnQuZG9jdW1lbnRFbGVtZW50LmFwcGVuZENoaWxkKHNjcmlwdCk7XHJcbiAgICAgIFVSTC5yZXZva2VPYmplY3RVUkwodXJsKTtcclxuICAgIH1cclxuICAgIGVsc2VcclxuICAgIHtcclxuICAgICAgc2NyaXB0LnRleHRDb250ZW50ID0gY29kZTtcclxuICAgICAgZG9jdW1lbnQuZG9jdW1lbnRFbGVtZW50LmFwcGVuZENoaWxkKHNjcmlwdCk7XHJcbiAgICB9XHJcblxyXG4gICAgZG9jdW1lbnQuZG9jdW1lbnRFbGVtZW50LnJlbW92ZUNoaWxkKHNjcmlwdCk7XHJcbiAgfVxyXG59XHJcbiIsIi8vIFRoZSBtb2R1bGUgY2FjaGVcbnZhciBfX3dlYnBhY2tfbW9kdWxlX2NhY2hlX18gPSB7fTtcblxuLy8gVGhlIHJlcXVpcmUgZnVuY3Rpb25cbmZ1bmN0aW9uIF9fd2VicGFja19yZXF1aXJlX18obW9kdWxlSWQpIHtcblx0Ly8gQ2hlY2sgaWYgbW9kdWxlIGlzIGluIGNhY2hlXG5cdGlmKF9fd2VicGFja19tb2R1bGVfY2FjaGVfX1ttb2R1bGVJZF0pIHtcblx0XHRyZXR1cm4gX193ZWJwYWNrX21vZHVsZV9jYWNoZV9fW21vZHVsZUlkXS5leHBvcnRzO1xuXHR9XG5cdC8vIENyZWF0ZSBhIG5ldyBtb2R1bGUgKGFuZCBwdXQgaXQgaW50byB0aGUgY2FjaGUpXG5cdHZhciBtb2R1bGUgPSBfX3dlYnBhY2tfbW9kdWxlX2NhY2hlX19bbW9kdWxlSWRdID0ge1xuXHRcdC8vIG5vIG1vZHVsZS5pZCBuZWVkZWRcblx0XHQvLyBubyBtb2R1bGUubG9hZGVkIG5lZWRlZFxuXHRcdGV4cG9ydHM6IHt9XG5cdH07XG5cblx0Ly8gRXhlY3V0ZSB0aGUgbW9kdWxlIGZ1bmN0aW9uXG5cdF9fd2VicGFja19tb2R1bGVzX19bbW9kdWxlSWRdKG1vZHVsZSwgbW9kdWxlLmV4cG9ydHMsIF9fd2VicGFja19yZXF1aXJlX18pO1xuXG5cdC8vIFJldHVybiB0aGUgZXhwb3J0cyBvZiB0aGUgbW9kdWxlXG5cdHJldHVybiBtb2R1bGUuZXhwb3J0cztcbn1cblxuIiwiLy8gZGVmaW5lIGdldHRlciBmdW5jdGlvbnMgZm9yIGhhcm1vbnkgZXhwb3J0c1xuX193ZWJwYWNrX3JlcXVpcmVfXy5kID0gKGV4cG9ydHMsIGRlZmluaXRpb24pID0+IHtcblx0Zm9yKHZhciBrZXkgaW4gZGVmaW5pdGlvbikge1xuXHRcdGlmKF9fd2VicGFja19yZXF1aXJlX18ubyhkZWZpbml0aW9uLCBrZXkpICYmICFfX3dlYnBhY2tfcmVxdWlyZV9fLm8oZXhwb3J0cywga2V5KSkge1xuXHRcdFx0T2JqZWN0LmRlZmluZVByb3BlcnR5KGV4cG9ydHMsIGtleSwgeyBlbnVtZXJhYmxlOiB0cnVlLCBnZXQ6IGRlZmluaXRpb25ba2V5XSB9KTtcblx0XHR9XG5cdH1cbn07IiwiX193ZWJwYWNrX3JlcXVpcmVfXy5vID0gKG9iaiwgcHJvcCkgPT4gT2JqZWN0LnByb3RvdHlwZS5oYXNPd25Qcm9wZXJ0eS5jYWxsKG9iaiwgcHJvcCkiLCIvLyBzdGFydHVwXG4vLyBMb2FkIGVudHJ5IG1vZHVsZVxuLy8gVGhpcyBlbnRyeSBtb2R1bGUgaXMgcmVmZXJlbmNlZCBieSBvdGhlciBtb2R1bGVzIHNvIGl0IGNhbid0IGJlIGlubGluZWRcbl9fd2VicGFja19yZXF1aXJlX18oMzM0KTtcbl9fd2VicGFja19yZXF1aXJlX18oNDg3KTtcbl9fd2VicGFja19yZXF1aXJlX18oNjU5KTtcbiJdLCJtYXBwaW5ncyI6Ijs7Ozs7OztBQUFBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBOzs7Ozs7QUN0T0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QTs7Ozs7O0FDbi9CQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFFQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBOzs7Ozs7Ozs7OztBQ3RsQkE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBRUE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QTs7Ozs7QUMxYkE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QTs7OztBQ3hXQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBOzs7O0FDdEJBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7Ozs7O0FDUEE7Ozs7QUNBQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7O0EiLCJzb3VyY2VSb290IjoiIn0=
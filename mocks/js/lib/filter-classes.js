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

"use strict";

const {params} = require("../config/env");

class Filter
{
  static fromText(text)
  {
    if (params.filterError)
      return new InvalidFilter(text, "filter_invalid_csp");

    if (text[0] === "!")
      return new CommentFilter(text);

    return new URLFilter(text);
  }

  static normalize(text)
  {
    return text;
  }

  constructor(text)
  {
    this.text = text;
  }
}

class ActiveFilter extends Filter
{
  constructor(text)
  {
    super(text);
    this.disabled = false;
  }
}

class CommentFilter extends Filter {}

class InvalidFilter extends Filter
{
  constructor(text, reason)
  {
    super(text);
    this.reason = reason;
  }
}

class URLFilter extends ActiveFilter {}

module.exports = {
  ActiveFilter,
  Filter,
  InvalidFilter,
  URLFilter
};

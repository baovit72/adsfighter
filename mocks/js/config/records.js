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

const records = [
  // blocked request
  {
    request: {
      url: "http://adserver.example.com/ad_banner.png",
      type: "IMAGE",
      docDomain: "example.com"
    },
    filter: {
      text: "/ad_banner*$domain=example.com",
      whitelisted: false,
      userDefined: false,
      subscription: "EasyList"
    }
  },
  // whitelisted request
  {
    request: {
      url: "http://example.com/looks_like_an_ad_but_isnt_one.html",
      type: "SUBDOCUMENT",
      docDomain: "example.com"
    },
    filter: {
      text: "@@||example.com/looks_like_an_ad_but_isnt_one.html",
      whitelisted: true,
      userDefined: false,
      subscription: "EasyList"
    }
  },
  // request with long URL and no filter matches
  {
    request: {
      url: "https://this.url.has.a.long.domain/and_a_long_path_maybe_not_long_enough_so_i_keep_typing?there=are&a=couple&of=parameters&as=well&and=even&some=more",
      type: "XMLHTTPREQUEST",
      docDomain: "example.com"
    },
    filter: null
  },
  // matching element hiding filter
  {
    request: {
      type: "ELEMHIDE",
      docDomain: "example.com"
    },
    filter: {
      text: "example.com##.ad_banner",
      whitelisted: false,
      userDefined: false,
      subscription: "EasyList"
    }
  },
  // user-defined filter
  {
    request: {
      url: "http://example.com/some-annoying-popup",
      type: "POPUP",
      docDomain: "example.com"
    },
    filter: {
      text: "||example.com/some-annoying-popup$popup",
      whitelisted: false,
      userDefined: true,
      subscription: null
    }
  },
  // rewrite
  {
    request: {
      url: "http://example.com/some-annoying-popup",
      type: "OTHER",
      docDomain: "example.com",
      rewrittenUrl: "http://example.com/some-annoying-popup?nopopup"
    },
    filter: {
      text: "/(example\\.com\\/some-annoying-popup\\)$/$rewrite=$1?nopopup",
      whitelisted: false,
      userDefined: true,
      subscription: null
    }
  },
  // long filter
  {
    request: {
      type: "ELEMHIDE",
      docDomain: "example.com"
    },
    filter: {
      text: `example.com${",example.com".repeat(499)}##.ad_banner`,
      whitelisted: false,
      userDefined: false,
      subscription: "EasyList"
    }
  }
];

module.exports = records;

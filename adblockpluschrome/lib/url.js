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

/** @module url */

/**
 * Gets the IDN-decoded hostname from the URL of a frame.
 * If the URL don't have host information (like "about:blank"
 * and "data:" URLs) it falls back to the parent frame.
 *
 * @param {?Frame}  frame
 * @param {URL}    [originUrl]
 * @return {string}
 */
export function extractHostFromFrame(frame, originUrl)
{
  for (; frame; frame = frame.parent)
  {
    let {hostname} = frame.url;
    if (hostname)
      return hostname;
  }

  return originUrl ? originUrl.hostname : "";
}

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

const {initDriver} = require("./browsers/chromium");

async function waitForExtension()
{
  const driver = await initDriver();
  let origin;
  await driver.wait(async() =>
  {
    for (const handle of await driver.getAllWindowHandles())
    {
      await driver.switchTo().window(handle);
      origin = await driver.executeAsyncScript(`
        let callback = arguments[arguments.length - 1];
        (async() =>
        {
          if (typeof browser != "undefined")
          {
            let info = await browser.management.getSelf();
            if (info.optionsUrl)
            {
              callback(location.origin);
              return;
            }
          }
          callback(null);
        })();`);
      if (origin)
        return true;
    }
    return false;
  }, 5000, "options page not found");

  return [driver, origin];
}

module.exports = {waitForExtension};

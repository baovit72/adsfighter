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

import {resolve} from "path";
import fs from "fs";
import {Readable} from "stream";
import Vinyl from "vinyl";

let manifest;

function editManifest(data, version, channel, target)
{
  data.version = version;
  data.name = `__MSG_name_${channel == "development" ? "dev" : channel}build__`;

  if (target == "chrome")
    delete data.applications;

  if (target == "firefox")
  {
    let {gecko} = data.applications;

    if (channel == "development")
      gecko.id = gecko.app_id_devbuild;
    else
      gecko.id = gecko.app_id_release;

    delete gecko.app_id_devbuild;
    delete gecko.app_id_release;

    delete data.minimum_chrome_version;
    delete data.minimum_opera_version;
    delete data.browser_action.default_popup;
    delete data.optional_permissions;
    delete data.storage;

    data.applications.gecko = gecko;
  }

  return data;
}

export function createManifest(contents)
{
  return new Readable.from([
    new Vinyl({
      contents: Buffer.from(JSON.stringify(contents, null, 2)),
      path: "manifest.json"
    })
  ]);
}

export async function getManifestContent({target, version, channel, path})
{
  if (manifest)
    return manifest;

  let raw = JSON.parse(
    await fs.promises.readFile(resolve(path || "build/manifest.json"))
  );

  manifest = editManifest(raw, version, channel, target);

  return manifest;
}

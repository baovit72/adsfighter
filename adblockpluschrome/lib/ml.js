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

import {port} from "./messaging.js";
import {ML} from "../adblockpluscore/lib/ml.js";

import * as tfCore from "@tensorflow/tfjs-core";
import * as tfConverter from "@tensorflow/tfjs-converter";

let tf = {};

for (let object of [tfCore, tfConverter])
{
  for (let property in object)
  {
    if (!Object.prototype.hasOwnProperty.call(tf, property))
      tf[property] = object[property];
  }
}

let mlByModel = new Map([
  ["mlHideIfGraphMatches", new ML(tf)]
]);

for (let [key, value] of mlByModel)
  value.modelURL = browser.runtime.getURL(`data/${key}/model.json`);

/**
 * Returns the inference on a ML model.
 *
 * @event "ml.inference"
 * @property {string} model Name of the model to use
 * @returns {Array.<number>}
 */
port.on("ml.inference", (message, sender) =>
{
  let ml = mlByModel.get(message.model);
  if (!ml)
    return Promise.reject(new Error("Model not found."));

  return ml.predict(message.inputs);
});

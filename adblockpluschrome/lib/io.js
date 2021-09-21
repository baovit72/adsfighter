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

const keyPrefix = "file:";

function fileToKey(fileName)
{
  return keyPrefix + fileName;
}

async function loadFile(fileName)
{
  let key = fileToKey(fileName);
  let items = await browser.storage.local.get(key);
  let entry = items[key];
  if (entry)
    return entry;
  throw {type: "NoSuchFile"};
}

function saveFile(fileName, data)
{
  return browser.storage.local.set({
    [fileToKey(fileName)]: {
      content: Array.from(data),
      lastModified: Date.now()
    }
  });
}

export let IO =
{
  /**
   * Reads text lines from a file.
   * @param {string} fileName
   *    Name of the file to be read
   * @param {TextSink} listener
   *    Function that will be called for each line in the file
   * @return {Promise}
   *    Promise to be resolved or rejected once the operation is completed
   */
  async readFromFile(fileName, listener)
  {
    let entry = await loadFile(fileName);
    for (let line of entry.content)
      listener(line);
  },

  /**
   * Writes text lines to a file.
   * @param {string} fileName
   *    Name of the file to be written
   * @param {Iterable.<string>} data
   *    An array-like or iterable object containing the lines (without line
   *    endings)
   * @return {Promise}
   *    Promise to be resolved or rejected once the operation is completed
   */
  writeToFile(fileName, data)
  {
    return saveFile(fileName, data);
  },

  /**
   * Renames a file.
   * @param {string} fromFile
   *    Name of the file to be renamed
   * @param {string} newName
   *    New file name, will be overwritten if exists
   * @return {Promise}
   *    Promise to be resolved or rejected once the operation is completed
   */
  async renameFile(fromFile, newName)
  {
    let entry = await loadFile(fromFile);
    await browser.storage.local.set({[fileToKey(newName)]: entry});
    await browser.storage.local.remove(fileToKey(fromFile));
  },

  /**
   * Retrieves file metadata.
   * @param {string} fileName
   *    Name of the file to be looked up
   * @return {Promise.<StatData>}
   *    Promise to be resolved with file metadata once the operation is
   *    completed
   */
  async statFile(fileName)
  {
    try
    {
      let entry = await loadFile(fileName);
      return {
        exists: true,
        lastModified: entry.lastModified
      };
    }
    catch (error)
    {
      if (error.type == "NoSuchFile")
        return {exists: false};
      throw error;
    }
  }
};

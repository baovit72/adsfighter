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

export {createManifest, getManifestContent} from "./manifest.js";
export {default as webpack} from "./webpack.js";
export {default as mapping} from "./mapping.js";
export {translations, chromeTranslations} from "./translations.js";
export {addDevEnvVersion, addUnitTestsPage} from "./devenv.js";
export {buildUI} from "./ui-dependency.js";
export {buildSnippets} from "./snippets-dependency.js";
export {default as sourceDistribution} from "./sourceDistribution.js";

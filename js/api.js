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

const platformToStore = new Map([
  ["chromium", "chrome"],
  ["edgehtml", "edge"],
  ["gecko", "firefox"]
]);

function getInfo()
{
  return Promise.all([
    app.get("application"),
    app.get("platform")
  ])
  .then(([application, platform]) =>
  {
    let store = application;
    // Edge and Opera have their own stores so we should refer to those instead
    if (application !== "edge" && application !== "opera")
    {
      store = platformToStore.get(platform) || "chrome";
    }

    return {application, platform, store};
  });
}

function send(type, args)
{
  args = Object.assign({}, {type}, args);
  return browser.runtime.sendMessage(args);
}

const app = {
  get: (what) => send("app.get", {what}),
  getInfo,
  open: (what) => send("app.open", {what})
};
module.exports.app = app;

const doclinks = {
  get: (link) => send("app.get", {what: "doclink", link})
};
module.exports.doclinks = doclinks;

const notifications = {
  get: (displayMethod) => send("notifications.get", {displayMethod}),
  seen: () => send("notifications.seen")
};
module.exports.notifications = notifications;

const prefs = {
  get: (key) => send("prefs.get", {key})
};
module.exports.prefs = prefs;

const subscriptions = {
  getInitIssues: () => send("subscriptions.getInitIssues")
};
module.exports.subscriptions = subscriptions;

const stats = {
  getBlockedPerPage: (tab) => send("stats.getBlockedPerPage", {tab}),
  getBlockedTotal: () => send("stats.getBlockedTotal")
};
module.exports.stats = stats;

// For now we are merely reusing the port for long-lived communications to fix
// https://gitlab.com/eyeo/adblockplus/abpui/adblockplusui/issues/415
const port = browser.runtime.connect({name: "ui"});
module.exports.port = port;

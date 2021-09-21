/******/ (() => { // webpackBootstrap
/******/ 	"use strict";
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

if (document instanceof HTMLDocument)
{
  document.addEventListener("click", event =>
  {
    // Ignore right-clicks
    if (event.button == 2)
      return;

    // Ignore simulated clicks.
    if (event.isTrusted == false)
      return;

    // Search the link associated with the click
    let link = event.target;
    while (!(link instanceof HTMLAnchorElement))
    {
      link = link.parentNode;

      if (!link)
        return;
    }

    let queryString = null;
    if (link.protocol == "http:" || link.protocol == "https:")
    {
      if (link.host == "subscribe.adblockplus.org" && link.pathname == "/")
        queryString = link.search.substr(1);
    }
    else
    {
      // Firefox 51 doesn't seem to populate the "search" property for
      // links with non-standard URL schemes so we need to extract the query
      // string manually.
      let match = /^abp:\/*subscribe\/*\?(.*)/i.exec(link.href);
      if (match)
        queryString = match[1];
    }

    if (!queryString)
      return;

    // This is our link - make sure the browser doesn't handle it
    event.preventDefault();
    event.stopPropagation();

    // Decode URL parameters
    let title = null;
    let url = null;
    for (let param of queryString.split("&"))
    {
      let parts = param.split("=", 2);
      if (parts.length != 2 || !/\S/.test(parts[1]))
        continue;
      switch (parts[0])
      {
        case "title":
          title = decodeURIComponent(parts[1]);
          break;
        case "location":
          url = decodeURIComponent(parts[1]);
          break;
      }
    }
    if (!url)
      return;

    // Default title to the URL
    if (!title)
      title = url;

    // Trim spaces in title and URL
    title = title.trim();
    url = url.trim();
    if (!/^(https?|ftp):/.test(url))
      return;

    browser.runtime.sendMessage({
      type: "subscriptions.add",
      title,
      url,
      confirm: true
    });
  }, true);
}

/******/ })()
;
//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic3Vic2NyaXB0aW9uTGluay5wb3N0bG9hZC5qcyIsInNvdXJjZXMiOlsid2VicGFjazovL2FkYmxvY2twbHVzY2hyb21lLy4vc3Vic2NyaXB0aW9uTGluay5wb3N0bG9hZC5qcyJdLCJzb3VyY2VzQ29udGVudCI6WyIvKlxyXG4gKiBUaGlzIGZpbGUgaXMgcGFydCBvZiBBZGJsb2NrIFBsdXMgPGh0dHBzOi8vYWRibG9ja3BsdXMub3JnLz4sXHJcbiAqIENvcHlyaWdodCAoQykgMjAwNi1wcmVzZW50IGV5ZW8gR21iSFxyXG4gKlxyXG4gKiBBZGJsb2NrIFBsdXMgaXMgZnJlZSBzb2Z0d2FyZTogeW91IGNhbiByZWRpc3RyaWJ1dGUgaXQgYW5kL29yIG1vZGlmeVxyXG4gKiBpdCB1bmRlciB0aGUgdGVybXMgb2YgdGhlIEdOVSBHZW5lcmFsIFB1YmxpYyBMaWNlbnNlIHZlcnNpb24gMyBhc1xyXG4gKiBwdWJsaXNoZWQgYnkgdGhlIEZyZWUgU29mdHdhcmUgRm91bmRhdGlvbi5cclxuICpcclxuICogQWRibG9jayBQbHVzIGlzIGRpc3RyaWJ1dGVkIGluIHRoZSBob3BlIHRoYXQgaXQgd2lsbCBiZSB1c2VmdWwsXHJcbiAqIGJ1dCBXSVRIT1VUIEFOWSBXQVJSQU5UWTsgd2l0aG91dCBldmVuIHRoZSBpbXBsaWVkIHdhcnJhbnR5IG9mXHJcbiAqIE1FUkNIQU5UQUJJTElUWSBvciBGSVRORVNTIEZPUiBBIFBBUlRJQ1VMQVIgUFVSUE9TRS4gIFNlZSB0aGVcclxuICogR05VIEdlbmVyYWwgUHVibGljIExpY2Vuc2UgZm9yIG1vcmUgZGV0YWlscy5cclxuICpcclxuICogWW91IHNob3VsZCBoYXZlIHJlY2VpdmVkIGEgY29weSBvZiB0aGUgR05VIEdlbmVyYWwgUHVibGljIExpY2Vuc2VcclxuICogYWxvbmcgd2l0aCBBZGJsb2NrIFBsdXMuICBJZiBub3QsIHNlZSA8aHR0cDovL3d3dy5nbnUub3JnL2xpY2Vuc2VzLz4uXHJcbiAqL1xyXG5cclxuaWYgKGRvY3VtZW50IGluc3RhbmNlb2YgSFRNTERvY3VtZW50KVxyXG57XHJcbiAgZG9jdW1lbnQuYWRkRXZlbnRMaXN0ZW5lcihcImNsaWNrXCIsIGV2ZW50ID0+XHJcbiAge1xyXG4gICAgLy8gSWdub3JlIHJpZ2h0LWNsaWNrc1xyXG4gICAgaWYgKGV2ZW50LmJ1dHRvbiA9PSAyKVxyXG4gICAgICByZXR1cm47XHJcblxyXG4gICAgLy8gSWdub3JlIHNpbXVsYXRlZCBjbGlja3MuXHJcbiAgICBpZiAoZXZlbnQuaXNUcnVzdGVkID09IGZhbHNlKVxyXG4gICAgICByZXR1cm47XHJcblxyXG4gICAgLy8gU2VhcmNoIHRoZSBsaW5rIGFzc29jaWF0ZWQgd2l0aCB0aGUgY2xpY2tcclxuICAgIGxldCBsaW5rID0gZXZlbnQudGFyZ2V0O1xyXG4gICAgd2hpbGUgKCEobGluayBpbnN0YW5jZW9mIEhUTUxBbmNob3JFbGVtZW50KSlcclxuICAgIHtcclxuICAgICAgbGluayA9IGxpbmsucGFyZW50Tm9kZTtcclxuXHJcbiAgICAgIGlmICghbGluaylcclxuICAgICAgICByZXR1cm47XHJcbiAgICB9XHJcblxyXG4gICAgbGV0IHF1ZXJ5U3RyaW5nID0gbnVsbDtcclxuICAgIGlmIChsaW5rLnByb3RvY29sID09IFwiaHR0cDpcIiB8fCBsaW5rLnByb3RvY29sID09IFwiaHR0cHM6XCIpXHJcbiAgICB7XHJcbiAgICAgIGlmIChsaW5rLmhvc3QgPT0gXCJzdWJzY3JpYmUuYWRibG9ja3BsdXMub3JnXCIgJiYgbGluay5wYXRobmFtZSA9PSBcIi9cIilcclxuICAgICAgICBxdWVyeVN0cmluZyA9IGxpbmsuc2VhcmNoLnN1YnN0cigxKTtcclxuICAgIH1cclxuICAgIGVsc2VcclxuICAgIHtcclxuICAgICAgLy8gRmlyZWZveCA1MSBkb2Vzbid0IHNlZW0gdG8gcG9wdWxhdGUgdGhlIFwic2VhcmNoXCIgcHJvcGVydHkgZm9yXHJcbiAgICAgIC8vIGxpbmtzIHdpdGggbm9uLXN0YW5kYXJkIFVSTCBzY2hlbWVzIHNvIHdlIG5lZWQgdG8gZXh0cmFjdCB0aGUgcXVlcnlcclxuICAgICAgLy8gc3RyaW5nIG1hbnVhbGx5LlxyXG4gICAgICBsZXQgbWF0Y2ggPSAvXmFicDpcXC8qc3Vic2NyaWJlXFwvKlxcPyguKikvaS5leGVjKGxpbmsuaHJlZik7XHJcbiAgICAgIGlmIChtYXRjaClcclxuICAgICAgICBxdWVyeVN0cmluZyA9IG1hdGNoWzFdO1xyXG4gICAgfVxyXG5cclxuICAgIGlmICghcXVlcnlTdHJpbmcpXHJcbiAgICAgIHJldHVybjtcclxuXHJcbiAgICAvLyBUaGlzIGlzIG91ciBsaW5rIC0gbWFrZSBzdXJlIHRoZSBicm93c2VyIGRvZXNuJ3QgaGFuZGxlIGl0XHJcbiAgICBldmVudC5wcmV2ZW50RGVmYXVsdCgpO1xyXG4gICAgZXZlbnQuc3RvcFByb3BhZ2F0aW9uKCk7XHJcblxyXG4gICAgLy8gRGVjb2RlIFVSTCBwYXJhbWV0ZXJzXHJcbiAgICBsZXQgdGl0bGUgPSBudWxsO1xyXG4gICAgbGV0IHVybCA9IG51bGw7XHJcbiAgICBmb3IgKGxldCBwYXJhbSBvZiBxdWVyeVN0cmluZy5zcGxpdChcIiZcIikpXHJcbiAgICB7XHJcbiAgICAgIGxldCBwYXJ0cyA9IHBhcmFtLnNwbGl0KFwiPVwiLCAyKTtcclxuICAgICAgaWYgKHBhcnRzLmxlbmd0aCAhPSAyIHx8ICEvXFxTLy50ZXN0KHBhcnRzWzFdKSlcclxuICAgICAgICBjb250aW51ZTtcclxuICAgICAgc3dpdGNoIChwYXJ0c1swXSlcclxuICAgICAge1xyXG4gICAgICAgIGNhc2UgXCJ0aXRsZVwiOlxyXG4gICAgICAgICAgdGl0bGUgPSBkZWNvZGVVUklDb21wb25lbnQocGFydHNbMV0pO1xyXG4gICAgICAgICAgYnJlYWs7XHJcbiAgICAgICAgY2FzZSBcImxvY2F0aW9uXCI6XHJcbiAgICAgICAgICB1cmwgPSBkZWNvZGVVUklDb21wb25lbnQocGFydHNbMV0pO1xyXG4gICAgICAgICAgYnJlYWs7XHJcbiAgICAgIH1cclxuICAgIH1cclxuICAgIGlmICghdXJsKVxyXG4gICAgICByZXR1cm47XHJcblxyXG4gICAgLy8gRGVmYXVsdCB0aXRsZSB0byB0aGUgVVJMXHJcbiAgICBpZiAoIXRpdGxlKVxyXG4gICAgICB0aXRsZSA9IHVybDtcclxuXHJcbiAgICAvLyBUcmltIHNwYWNlcyBpbiB0aXRsZSBhbmQgVVJMXHJcbiAgICB0aXRsZSA9IHRpdGxlLnRyaW0oKTtcclxuICAgIHVybCA9IHVybC50cmltKCk7XHJcbiAgICBpZiAoIS9eKGh0dHBzP3xmdHApOi8udGVzdCh1cmwpKVxyXG4gICAgICByZXR1cm47XHJcblxyXG4gICAgYnJvd3Nlci5ydW50aW1lLnNlbmRNZXNzYWdlKHtcclxuICAgICAgdHlwZTogXCJzdWJzY3JpcHRpb25zLmFkZFwiLFxyXG4gICAgICB0aXRsZSxcclxuICAgICAgdXJsLFxyXG4gICAgICBjb25maXJtOiB0cnVlXHJcbiAgICB9KTtcclxuICB9LCB0cnVlKTtcclxufVxyXG4iXSwibWFwcGluZ3MiOiI7O0FBQUE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTs7O0EiLCJzb3VyY2VSb290IjoiIn0=
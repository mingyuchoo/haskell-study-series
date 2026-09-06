/* Initialization only. All application state, events and rendering live in Elm. */
'use strict';
const now = new Date();
Elm.Main.init({
  node: document.getElementById('app'),
  flags: {
    seed: crypto.randomUUID(),
    today: now.toISOString().slice(0, 10),
    deadline: new Date(now.getTime() + 90 * 86400000).toISOString().slice(0, 10)
  }
});

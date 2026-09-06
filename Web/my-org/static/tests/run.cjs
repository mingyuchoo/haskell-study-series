// elm-test starts one worker per CPU. Bound memory use on large CI/dev hosts.
const os = require('node:os');
const cpus = os.cpus;
os.cpus = () => cpus().slice(0, 2);
require('elm-test/lib/elm-test.js');

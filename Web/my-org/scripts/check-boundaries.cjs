// Architectural dependency rules are checked without compiling generated code.
const fs = require('node:fs');
const path = require('node:path');
const root = path.resolve(__dirname, '..');
function files(dir, suffix) {
  return fs.readdirSync(dir, { withFileTypes: true }).flatMap(entry => {
    const file = path.join(dir, entry.name);
    return entry.isDirectory() ? files(file, suffix) : file.endsWith(suffix) ? [file] : [];
  });
}
const violations = [];
for (const file of files(path.join(root, 'static/src'), '.elm')) {
  const source = fs.readFileSync(file, 'utf8');
  const name = source.match(/^module\s+([\w.]+)/m)?.[1];
  const imports = [...source.matchAll(/^import\s+([\w.]+)/gm)].map(match => match[1]);
  let forbidden;
  if (/^(Domain|Form)(\.|$)/.test(name)) forbidden = /^(Main|Api|Page|Ui|Http|Browser|Task)(\.|$)/;
  else if (/^Api(\.|$)/.test(name)) forbidden = /^(Main|Page|Ui)(\.|$)/;
  else if (/^(Page|Ui)(\.|$)/.test(name)) forbidden = /^(Main|Api\.Http|Http|Browser|Task)(\.|$)/;
  if (forbidden) {
    for (const imported of imports.filter(value => forbidden.test(value))) {
      violations.push(`${name} must not import ${imported}`);
    }
  }
}
// Pure application calculations must not depend on their runtime or transports.
for (const file of files(path.join(root, 'src'), '.hs')) {
  const source = fs.readFileSync(file, 'utf8');
  const name = source.match(/^module\s+([\w.]+)/m)?.[1];
  const imports = [...source.matchAll(/^import\s+(?:qualified\s+)?([\w.]+)/gm)].map(match => match[1]);
  const pureCore = /^MyOrg\.(Domain(\.|$)|Application$|Application\.(Plan|Query|ReadModel)$|Registry$|Demo$)/.test(name);
  if (pureCore) {
    const forbidden = /^(Data\.Aeson|Network|Database|System|Control\.Concurrent|MyOrg\.(Server|Store|Http|Infrastructure|Transport|Serialization)|MyOrg\.Application\.(Runtime|Persistence))(\.|$)/;
    for (const imported of imports.filter(value => forbidden.test(value))) {
      violations.push(`${name} must not import ${imported}`);
    }
    if (/\b(getCurrentTime|IO)\b/.test(source.replace(/--.*$/gm, ''))) {
      violations.push(`${name} must receive time and data as inputs without IO`);
    }
    if (name !== 'MyOrg.Domain.Event') {
      for (const imported of imports.filter(value => /^(MyOrg\.Types|MyOrg\.Domain\.Event)$/.test(value))) {
        violations.push(`${name} must import focused domain modules instead of ${imported}`);
      }
    }
  }
  if (/^MyOrg\.Domain\./.test(name) && name !== 'MyOrg.Domain.Event') {
    for (const imported of imports.filter(value => /^MyOrg\.(Application|Presentation)(\.|$)/.test(value))) {
      violations.push(`${name} must not depend on outer layer ${imported}`);
    }
  }
  const stateLayers = {
    'MyOrg.Domain.State': /^MyOrg\.Domain\.(Queries|Validation|Reducer)$/,
    'MyOrg.Domain.Queries': /^MyOrg\.Domain\.(Validation|Reducer)$/,
    'MyOrg.Domain.Validation': /^MyOrg\.Domain\.Reducer$/,
  };
  if (stateLayers[name]) {
    for (const imported of imports.filter(value => stateLayers[name].test(value))) {
      violations.push(`${name} must not reverse the state dependency direction through ${imported}`);
    }
  }
}
if (violations.length) {
  console.error(violations.join('\n'));
  process.exitCode = 1;
} else {
  console.log('Architectural dependency boundaries passed.');
}

import { createReadStream } from 'fs';
import { createInterface } from 'readline';

try {
  let inputPath = '', isV2 = false, isDebug = false;
  for (const arg of process.argv.slice(2)) {
    if (arg === '-v1') { isV2 = false; continue; }
    if (arg === '-v2') { isV2 = true; continue; }
    if (arg === '-d') { isDebug = true; continue; }
    if (arg.startsWith('-')) { console.warn('\x1b[43m', 'Invalid flag:', arg, '\x1b[0m'); continue; }
    if (inputPath !== '') { console.warn('\x1b[43m', 'Multiple inputs defined. Ignoring', arg, '\x1b[0m'); continue; };
    inputPath = arg;
  }
  if (inputPath === '') throw Error('No input file given.');

  const dict = {
    1: 1, 2: 2, 3: 3, 4: 4, 5: 5, 6: 6, 7: 7, 8: 8, 9: 9,
    ...(isV2 && { one: 1, two: 2, three: 3, four: 4, five: 5, six: 6, seven: 7, eight: 8, nine: 9 }),
  };

  let result = 0;
  const search = new RegExp(`(?=(${Object.keys(dict).join('|')}))`, 'g');
  const lines = createInterface(createReadStream(inputPath));
  for await (const line of lines) {
    const matches = [...line.matchAll(search, {})].map((groups) => groups[1]);
    if (!matches || matches.length === 0) {
      console.warn('\x1b[43m', `Invalid line:`.padEnd(21), line, '\x1b[0m');
      continue;
    }
    if (isDebug) {
      console.debug('\x1b[90m', matches.at(0).padEnd(10), matches.at(-1).padEnd(10), line, '\x1b[0m');
    }
    result += dict[matches.at(0)] * 10;
    result += dict[matches.at(-1)];
  }

  console.log('The result is:', result);

} catch (err) {
  console.info(`Usage: node ./main.mjs [-v1|-v2|-d] inputFile\n`)
  console.error('\x1b[41m', err.message, '\x1b[0m');
  console.info('Exiting with code 1.');
  process.exit(1);
}

import { parse } from 'https://deno.land/std@0.204.0/flags/mod.ts';
import { App } from './app.ts';

function main() {
    const flags = parse(Deno.args, {
        boolean: ['update', 'summary', 'json'],
        string: ['champion', 'advice', 'timeOfDay'],
    });
    if (!flags.update && !flags.summary && !flags.champion && !flags.advice && !flags.timeOfDay) {
        console.log('No flags recognized');
        return -1;
    }
    new App(
        flags.update,
        flags.summary,
        flags.champion,
        flags.advice,
        flags.json,
        flags.timeOfDay,
    ).run();
}

main();
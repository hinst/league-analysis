import { parse } from 'https://deno.land/std@0.204.0/flags/mod.ts';
import { App } from './app.ts';

function main() {
    const flags = parse(Deno.args, {
        boolean: ['update', 'summary'],
        string: ['champion', 'advice'],
    });
    if (!flags.update && !flags.summary && !flags.champion && !flags.advice) {
        console.log('No flags recognized');
        return -1;
    }
    new App(flags.update, flags.summary, flags.champion, flags.advice).run();
}

main();
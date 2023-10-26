import { parse } from 'https://deno.land/std@0.204.0/flags/mod.ts';
import { App } from './app.ts';

const flags = parse(Deno.args, {
    boolean: ['update', 'summary'],
    string: ['champion'],
});

new App(flags.update, flags.summary, flags.champion).run();
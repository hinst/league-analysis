class App {
    URL = ''
    async run() {
        const config = await Deno.readFile('./config.json');
    }
}

new App().run();
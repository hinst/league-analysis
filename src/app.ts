import { Config } from './config.ts';

class App {
    apiUrl = 'https://europe.api.riotgames.com';
    apiKey = '';

    async run() {
        const configFile = Deno.readTextFileSync('./config.json');
        const config = JSON.parse(configFile) as Config;
        this.apiKey = config.apiKey;
        const userId = await this.readUserId(config.gameName, config.tagLine);
        console.log(userId);
    }

    private async readUserId(gameName: string, tagLine: string) {
        const response = await fetch(
            this.apiUrl + '/riot/account/v1/accounts/by-riot-id/' +
            encodeURIComponent(gameName) + '/' +
            encodeURIComponent(tagLine) +
            '?api_key=' + encodeURIComponent(this.apiKey)
        );
        const responseObject = JSON.parse(await response.text());
        return responseObject.puuid;
    }
}

new App().run();
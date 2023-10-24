import { Config } from './config.ts';

const HTTP_STATUS_RATE_LIMIT_EXCEEDED = 429;

class App {
    apiUrl = 'https://europe.api.riotgames.com';
    apiKey = '';
    userId = '';;

    async run() {
        const configFile = Deno.readTextFileSync('./config.json');
        const config = JSON.parse(configFile) as Config;
        this.apiKey = config.apiKey;
        this.userId = await this.readUserId(config.gameName, config.tagLine);
        console.log(await this.readAllMatches());
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

    /** Read matches also known as battles */
    private async readMatches(startIndex: number) {
        const count = 100;
        const response = await fetch(
            this.apiUrl + '/lol/match/v5/matches/by-puuid/' +
            encodeURIComponent(this.userId) + '/ids' +
            '?api_key=' + encodeURIComponent(this.apiKey) +
            '&start=' + encodeURIComponent(startIndex) +
            '&count=' + encodeURIComponent(count)
        );
        const responseObject = JSON.parse(await response.text());
        return responseObject as string[];
    }

    private async readAllMatches() {
        const allMatches: string[] = [];
        let startIndex = 0;
        while (startIndex >= 0) {
            const matches = await this.readMatches(startIndex);
            if (matches.length) {
                allMatches.push(...matches);
                startIndex += matches.length;
            } else
                startIndex = -1;
        }
        return allMatches;
    }

    private async readMatch(matchId: string) {
        const response = await fetch(
            this.apiUrl + '/lol/match/v5/matches/' + encodeURIComponent(matchId) +
            '?api_key=' + encodeURIComponent(this.apiKey)
        );
        const responseObject = JSON.parse(await response.text());
        return responseObject;
    }
}

new App().run();
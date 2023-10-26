import { Config } from './config.ts';
import { MatchInfoMap, MatchInfoRecord, MatchParticipantInfo, findChampions } from './match.ts';
import { Status } from 'https://deno.land/std@0.204.0/http/http_status.ts';
import { sleep } from './sleep.ts';
import { findByEditingDistance } from './editingDistance.ts';
import { ChampionWinRateInfo } from './championWinRate.ts';

export class App {
    apiUrl = 'https://europe.api.riotgames.com';
    apiKey = '';
    userId = '';
    matchInfoMap: MatchInfoMap = {};
    matchInfoMapFileName = 'matchInfoMap.json';

    constructor(
        private updateEnabled: boolean,
        private printSummaryEnabled: boolean,
        private championNameInput?: string,
    ) {
    }

    async run() {
        const configFile = Deno.readTextFileSync('./config.json');
        const config = JSON.parse(configFile) as Config;
        this.apiKey = config.apiKey;
        if (config.userId) {
            this.userId = config.userId;
        } else {
            this.userId = await this.readUserId(config.gameName, config.tagLine);
            config.userId = this.userId;
            Deno.writeTextFileSync('./config.json', JSON.stringify(config, null, 2));
        }
        this.readMatchInfoMap();
        if (this.updateEnabled) {
            const countOfUpdated = await this.updateMatchInfoMap();
            console.log('Updated [' + countOfUpdated + ']');
        }
        if (this.printSummaryEnabled)
            this.printSummary();
        if (this.championNameInput)
            this.printChampionSummary(this.championNameInput);
    }

    private async updateMatchInfoMap() {
        const allMatches = await this.readAllMatches();
        let countOfUpdated = 0;
        for (const matchId of allMatches) {
            if (!this.matchInfoMap[matchId]) {
                console.log(matchId);
                const matchInfo = await this.readMatch(matchId);
                this.matchInfoMap[matchId] = matchInfo;
                countOfUpdated += 1;
            }
        }
        this.writeMatchInfoMap();
        return countOfUpdated;
    }

    private printSummary() {
        const allMatches = Object.values(this.matchInfoMap);
        allMatches.sort((a, b) => a.info.gameCreation - b.info.gameCreation);
        console.log('Stored matches [' + allMatches.length + ']');
        if (allMatches.length > 0) {
            console.log('  oldest: ' + new Date(allMatches[0].info.gameCreation));
            console.log('  newest: ' + new Date(allMatches[allMatches.length - 1].info.gameCreation));
        }
        const champions = findChampions(allMatches);
        console.log('Champions [' + Object.keys(champions).length + ']');
        const playerChampions = findChampions(allMatches, this.userId);
        console.log('Your champions: ');
        for (const championName in playerChampions)
            console.log('  ' + championName, playerChampions[championName]);
    }

    private printChampionSummary(championNameInput: string) {
        const allMatches = Object.values(this.matchInfoMap);
        const yourChampions = findChampions(allMatches, this.userId);
        const championName = findByEditingDistance(Object.keys(yourChampions), championNameInput);
        if (championName) {
            console.log(championName + ' [' + yourChampions[championName] + ']');
            const stats = this.buildStats(championName);
            for (const stat of stats) {
                console.log(stat.toString());
            }
        } else {
            console.warn('Champion not found: ' + championNameInput);
        }
    }

    private readMatchInfoMap() {
        const dataFiles = Deno.readDirSync('./data');
        let matchInfoMapExists = false;
        for (const dataFile of dataFiles)
            if (dataFile.name === this.matchInfoMapFileName)
                matchInfoMapExists = true;
        if (matchInfoMapExists)
            this.matchInfoMap = JSON.parse(Deno.readTextFileSync('./data/' + this.matchInfoMapFileName));
    }

    private writeMatchInfoMap() {
        Deno.writeTextFileSync('./data/' + this.matchInfoMapFileName, JSON.stringify(this.matchInfoMap));
    }

    private async readUserId(gameName: string, tagLine: string) {
        const responseObject = await this.fetchFromApi(
            this.apiUrl + '/riot/account/v1/accounts/by-riot-id/' +
            encodeURIComponent(gameName) + '/' +
            encodeURIComponent(tagLine) +
            '?api_key=' + encodeURIComponent(this.apiKey)
        );
        return responseObject.puuid;
    }

    /** Read matches also known as battles */
    private async readMatches(startIndex: number) {
        const count = 100;
        return await this.fetchFromApi(
            this.apiUrl + '/lol/match/v5/matches/by-puuid/' +
            encodeURIComponent(this.userId) + '/ids' +
            '?api_key=' + encodeURIComponent(this.apiKey) +
            '&start=' + encodeURIComponent(startIndex) +
            '&count=' + encodeURIComponent(count)
        ) as string[];
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
        return await this.fetchFromApi(
            this.apiUrl + '/lol/match/v5/matches/' + encodeURIComponent(matchId) +
            '?api_key=' + encodeURIComponent(this.apiKey)
        ) as MatchInfoRecord;
    }

    private async fetchFromApi(url: string): Promise<any> {
        const response = await fetch(url);
        if (!response.ok) {
            if (response.status === Status.Forbidden)
                console.warn('Please provide a fresh RIOT API token from https://developer.riotgames.com');
            if (response.status === Status.TooManyRequests) {
                await sleep(1000);
                return this.fetchFromApi(url);
            }
            throw new Error(response.statusText);
        }
        return response.json();
    }

    private buildStats(playerChampionName: string): ChampionWinRateInfo[] {
        const isDesiredParticipant = (participant: MatchParticipantInfo) =>
            participant.puuid === this.userId && participant.championName === playerChampionName;
        const matches = Object.values(this.matchInfoMap)
            .filter(match => match.info.participants.some(isDesiredParticipant));
        const winRateInfoMap: Record<string, ChampionWinRateInfo> = {};
        for (const match of matches) {
            const playerParticipant = match.info.participants.find(isDesiredParticipant);
            if (playerParticipant) {
                for (const participant of match.info.participants) {
                    let winRateItem = winRateInfoMap[participant.championName];
                    if (!winRateItem) {
                        winRateItem = new ChampionWinRateInfo(participant.championName);
                        winRateInfoMap[participant.championName] = winRateItem;
                    }
                    const isAlly = participant.teamId === playerParticipant.teamId;
                    if (isAlly) {
                        winRateItem.allyInfo.matchCount += 1;
                        if (participant.win)
                            winRateItem.allyInfo.victoryCount += 1;
                    } else {
                        winRateItem.enemyInfo.matchCount += 1;
                        if (participant.win)
                            winRateItem.enemyInfo.victoryCount += 1;
                    }
                }
            }
        }
        const winRateInfoArray = Object.values(winRateInfoMap);
        winRateInfoArray.sort((a, b) => b.totalMatchCount - a.totalMatchCount);
        return winRateInfoArray;
    }
}
import { Status } from 'https://deno.land/std@0.204.0/http/http_status.ts';
import { Config } from './config.ts';
import { MatchInfoMap, MatchInfoRecord, findChampions } from './match.ts';
import { sleep } from './sleep.ts';
import { findByEditingDistance } from './editingDistance.ts';
import { ChampionWinRateInfo, Team, WinRateInfo } from './championWinRate.ts';
import { formatPercent } from './format.ts';
import { sortObjectFieldsByName, sortObjectFieldsByNumber } from './object.ts';
import { TeamChanceAdvice } from './teamChanceAdvice.ts';

export class App {
    private apiUrl = 'https://europe.api.riotgames.com';
    private apiKey = '';
    private userId = '';
    private matchInfoMap: MatchInfoMap = {};
    private matchInfoMapFileName = 'matchInfoMap.json';
    private static readonly SIGNIFICANT_STATISTIC_THRESHOLD = 10;

    constructor(
        private updateEnabled: boolean,
        private printSummaryEnabled: boolean,
        private championNameInput?: string,
        private adviceQuery?: string,
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
        if (this.adviceQuery)
            this.printAdvice(this.adviceQuery);
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
        const allChampions = findChampions(allMatches);
        console.log('Champions [' + Object.keys(allChampions).length + ']');
        const userChampions = sortObjectFieldsByNumber(findChampions(allMatches, this.userId), -1);
        console.log('Your champions: ');
        for (const championName in userChampions)
            console.log('  ' + championName, userChampions[championName],
                formatPercent(WinRateInfo.getWinRate(allMatches, this.userId, championName).winRate));
    }

    private printChampionSummary(championNameInput: string) {
        const allMatches = Object.values(this.matchInfoMap);
        const yourChampions = findChampions(allMatches, this.userId);
        const championName = findByEditingDistance(Object.keys(yourChampions), championNameInput);
        if (championName) {
            console.log(championName + ' [' + yourChampions[championName] + '] ' +
                formatPercent(WinRateInfo.getWinRate(allMatches, this.userId, championName).winRate));
            const stats = ChampionWinRateInfo.build(allMatches, this.userId, championName);
            const bestAllies = ChampionWinRateInfo.sortTop(stats, App.SIGNIFICANT_STATISTIC_THRESHOLD, Team.ALLY, -1);
            console.log('Best allies: ');
            for (const bestAlly of bestAllies.slice(0, 5))
                console.log('  ' + bestAlly.toString(Team.ALLY));
            const worstAllies = ChampionWinRateInfo.sortTop(stats, App.SIGNIFICANT_STATISTIC_THRESHOLD, Team.ALLY, 1);
            console.log('Worst allies: ');
            for (const worstAlly of worstAllies.slice(0, 5))
                console.log('  ' + worstAlly.toString(Team.ALLY));
            const easiestEnemies = ChampionWinRateInfo.sortTop(stats, App.SIGNIFICANT_STATISTIC_THRESHOLD, Team.ENEMY, -1);
            console.log('Easiest enemies: ');
            for (const easiestEnemy of easiestEnemies.slice(0, 5))
                console.log('  ' + easiestEnemy.toString(Team.ENEMY));
            const hardestEnemies = ChampionWinRateInfo.sortTop(stats, App.SIGNIFICANT_STATISTIC_THRESHOLD, Team.ENEMY, 1);
            console.log('Hardest enemies: ');
            for (const hardestEnemy of hardestEnemies.slice(0, 5))
                console.log('  ' + hardestEnemy.toString(Team.ENEMY));
            const winRateMonths = sortObjectFieldsByName(
                ChampionWinRateInfo.getWinRateByMonth(allMatches, this.userId, championName), 1
            );
            console.log('Win rate by month: ');
            for (const winRateMonth in winRateMonths)
                console.log('  ' + winRateMonth, formatPercent(winRateMonths[winRateMonth].winRate));
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
        ) as { puuid: string };
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

    private async fetchFromApi<T>(url: string): Promise<T> {
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

    private printAdvice(adviceQuery: string) {
        const champions = adviceQuery.split(',');
        const allMatches = Object.values(this.matchInfoMap);
        const allChampions = Object.keys(findChampions(allMatches));
        const allies = champions
            .filter(championName => championName.startsWith('+'))
            .map(championName => championName.substring(1))
            .map(championName => findByEditingDistance(allChampions, championName))
            .filter(championName => championName !== undefined) as string[];
        const enemies = champions
            .filter(championName => championName.startsWith('-'))
            .map(championName => championName.substring(1))
            .map(championName => findByEditingDistance(allChampions, championName))
            .filter(championName => championName !== undefined) as string[];
        const userChampions = sortObjectFieldsByNumber(
            findChampions(Object.values(this.matchInfoMap), this.userId), -1
        );
        const advices = Object.keys(userChampions).map(userChampion => {
            const advice = new TeamChanceAdvice(userChampion);
            const stats = ChampionWinRateInfo.build(allMatches, this.userId, userChampion);
            for (const ally of allies) {
                const allyStats = stats.find(stat => stat.championName === ally && stat.allyInfo.matchCount > 0);
                if (allyStats)
                    advice.add(allyStats, Team.ALLY);
            }
            for (const enemy of enemies) {
                const enemyStats = stats.find(stat => stat.championName === enemy && stat.enemyInfo.matchCount > 0);
                if (enemyStats)
                    advice.add(enemyStats, Team.ENEMY);
            }
            return advice;
        });
        advices.sort((a, b) => (b.totalWinRate.winRate || 0) - (a.totalWinRate.winRate || 0));
        for (const advice of advices) {
            console.log(advice.championName + ' ' + advice.totalWinRate.toString());
            for (const champion of advice.champions)
                console.log('  ' + champion.championName + ' ' + champion.winRate.toString());
        }
    }
}
import { Team, WinRateInfo, ChampionWinRateInfo } from './championWinRate.ts';

export class TeamChanceAdvice {
    public totalWinRate: WinRateInfo = new WinRateInfo();
    public champions: ChampionChanceAdvice[] = [];

    public constructor(public championName: string) {
    }

    public add(info: ChampionWinRateInfo, team: Team) {
        const winRate = team === Team.ALLY ? info.allyInfo : info.enemyInfo;
        this.champions.push(new ChampionChanceAdvice(info.championName, team, winRate));
        this.totalWinRate.matchCount += winRate.matchCount;
        this.totalWinRate.victoryCount += winRate.victoryCount;
    }
}

export class ChampionChanceAdvice {
    public constructor(
        public championName: string,
        public team: Team,
        public winRate: WinRateInfo,
    ) {
    }
}
import { formatPercent } from './format.ts';
import { MatchInfoRecord, MatchParticipantInfo } from './match.ts';

export class WinRateInfo {
    constructor(
        public matchCount: number = 0,
        public victoryCount: number = 0,
    ) {
    }

    get winRate(): number | undefined {
        return this.matchCount ? this.victoryCount / this.matchCount : undefined;
    }

    toString() {
        return '' + this.victoryCount + '/' + this.matchCount + '=' + formatPercent(this.winRate);
    }

    static getWinRate(records: MatchInfoRecord[], userId: string, championName: string): WinRateInfo {
        let matchCount = 0;
        let victoryCount = 0;
        for (const record of records) {
            const participant = record.info.participants.find(p => p.puuid === userId && p.championName === championName);
            if (participant) {
                matchCount += 1;
                if (participant.win)
                    victoryCount += 1;
            }
        }
        return new WinRateInfo(matchCount, victoryCount);
    }
}

export enum Team {
    ALLY = 1,
    ENEMY = -1,
}

export class ChampionWinRateInfo {
    constructor(
        public championName: string,
        public allyInfo: WinRateInfo = new WinRateInfo(0, 0),
        public enemyInfo: WinRateInfo = new WinRateInfo(0, 0),
    ) {
    }

    get totalMatchCount(): number {
        return this.allyInfo.matchCount + this.enemyInfo.matchCount;
    }

    toString(team?: Team) {
        let text = this.championName + ' [' + this.totalMatchCount + ']';
        if (!team || team === Team.ALLY)
            text += ' ally ' + formatPercent(this.allyInfo.winRate);
        if (!team || team === Team.ENEMY)
            text += ' enemy ' + formatPercent(this.enemyInfo.winRate);
        return text;
    }

    static sortTop(infos: ChampionWinRateInfo[], significantStatisticThreshold: number, team: Team, sortDirection: number): ChampionWinRateInfo[] {
        function isSignificant(info: ChampionWinRateInfo) {
            return team === Team.ALLY
                ? info.allyInfo.matchCount >= significantStatisticThreshold
                : info.enemyInfo.matchCount >= significantStatisticThreshold;
        }
        function compare(a: ChampionWinRateInfo, b: ChampionWinRateInfo) {
            const comparison = team === Team.ALLY
                ? (a.allyInfo.winRate || 0) - (b.allyInfo.winRate || 0)
                : (a.enemyInfo.winRate || 0) - (b.enemyInfo.winRate || 0);
            return comparison * sortDirection;
        }
        return infos.filter(isSignificant).sort(compare);
    }

    static build(matches: MatchInfoRecord[], userId: string, playerChampionName: string): ChampionWinRateInfo[] {
        const isDesiredParticipant = (participant: MatchParticipantInfo) =>
            participant.puuid === userId && participant.championName === playerChampionName;
        const winRateInfoMap: Record<string, ChampionWinRateInfo> = {};
        for (const match of matches) {
            const player = match.info.participants.find(isDesiredParticipant);
            if (player) {
                for (const teammate of match.info.participants) {
                    if (teammate.puuid === userId)
                        continue;
                    let winRateItem = winRateInfoMap[teammate.championName];
                    if (!winRateItem) {
                        winRateItem = new ChampionWinRateInfo(teammate.championName);
                        winRateInfoMap[teammate.championName] = winRateItem;
                    }
                    const isAlly = teammate.teamId === player.teamId;
                    if (isAlly) {
                        winRateItem.allyInfo.matchCount += 1;
                        if (player.win)
                            winRateItem.allyInfo.victoryCount += 1;
                    } else {
                        winRateItem.enemyInfo.matchCount += 1;
                        if (player.win)
                            winRateItem.enemyInfo.victoryCount += 1;
                    }
                }
            }
        }
        const winRateInfoArray = Object.values(winRateInfoMap);
        winRateInfoArray.sort((a, b) => b.totalMatchCount - a.totalMatchCount);
        return winRateInfoArray;
    }

    static getWinRateByMonth(matches: MatchInfoRecord[], userId: string, playerChampionName: string) {
        const isDesiredParticipant = (participant: MatchParticipantInfo) =>
            participant.puuid === userId && participant.championName === playerChampionName;
        const monthlyWinRateMap: Record<string, WinRateInfo> = {};
        for (const match of matches) {
            const playerParticipant = match.info.participants.find(isDesiredParticipant);
            if (playerParticipant) {
                const monthKey = new Date(match.info.gameCreation).toISOString().slice(0, 7);
                let winRateInfo = monthlyWinRateMap[monthKey];
                if (!winRateInfo) {
                    winRateInfo = new WinRateInfo(0, 0);
                    monthlyWinRateMap[monthKey] = winRateInfo;
                }
                winRateInfo.matchCount += 1;
                if (playerParticipant.win)
                    winRateInfo.victoryCount += 1;
            }
        }
        return monthlyWinRateMap;
    }
}

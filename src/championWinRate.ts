import { formatPercent } from './format.ts';
import { MatchInfoRecord, MatchParticipantInfo } from './match.ts';

export class WinRateInfo {
    constructor(
        public matchCount: number,
        public victoryCount: number,
    ) {
    }

    get winRate(): number | undefined {
        return this.matchCount ? this.victoryCount / this.matchCount : undefined;
    }

    static getDualWinRate(records: MatchInfoRecord[], userId: string, userChampionName: string, secondChampionName: string, team: Team): WinRateInfo {
        let matchCount = 0;
        let victoryCount = 0;
        for (const record of records) {
            const player = record.info.participants.find(p => p.puuid === userId && p.championName === userChampionName);
            const teammate = record.info.participants.find(p => p.puuid !== userId && p.championName === secondChampionName);
            const isMatching = player && teammate &&
                (team === Team.ALLY && player.teamId === teammate.teamId || team === Team.ENEMY && player.teamId !== teammate.teamId);
            if (isMatching) {
                matchCount += 1;
                if (player.win)
                    victoryCount += 1;
            }
        }
        return new WinRateInfo(matchCount, victoryCount);
    }

    static getDualWinRateMap(records: MatchInfoRecord[], userId: string, userChampionName: string, secondChampionNames: string[], team: Team): Record<string, WinRateInfo> {
        const winRateMap: Record<string, WinRateInfo> = {};
        for (const secondChampionName of secondChampionNames)
            winRateMap[secondChampionName] = WinRateInfo.getDualWinRate(records, userId, userChampionName, secondChampionName, team);
        return winRateMap;
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

    toString() {
        return this.championName + ' [' + this.totalMatchCount + ']' +
            ' ally ' + formatPercent(this.allyInfo.winRate) +
            ' enemy ' + formatPercent(this.enemyInfo.winRate);
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

export function getWinRate(records: MatchInfoRecord[], userId: string, championName: string): number | undefined {
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
    return matchCount ? victoryCount / matchCount : undefined;
}

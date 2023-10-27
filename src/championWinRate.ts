import { formatPercent } from './format.ts';
import { MatchInfoRecord, MatchParticipantInfo } from './match.ts';

class WinRateInfo {
    constructor(
        public matchCount: number,
        public victoryCount: number,
    ) {
    }

    get winRate(): number | undefined {
        return this.matchCount === 0 ? undefined : this.victoryCount / this.matchCount;
    }
}

export enum Team {
    ALLY = 1,
    ENEMY = -1,
}

export class ChampionWinRateInfo {
    static readonly SIGNIFICANT_STATISTIC_THRESHOLD = 10;

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

    static sortTop(infos: ChampionWinRateInfo[], team: Team, sortDirection: number): ChampionWinRateInfo[] {
        function isSignificant(info: ChampionWinRateInfo) {
            return team === Team.ALLY
                ? info.allyInfo.matchCount >= ChampionWinRateInfo.SIGNIFICANT_STATISTIC_THRESHOLD
                : info.enemyInfo.matchCount >= ChampionWinRateInfo.SIGNIFICANT_STATISTIC_THRESHOLD;
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


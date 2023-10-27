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

    static sortTop(infos: ChampionWinRateInfo[], isAlly: boolean, isReverse: boolean): ChampionWinRateInfo[] {
        function isSignificant(info: ChampionWinRateInfo) {
            return isAlly
                ? info.allyInfo.matchCount >= ChampionWinRateInfo.SIGNIFICANT_STATISTIC_THRESHOLD
                : info.enemyInfo.matchCount >= ChampionWinRateInfo.SIGNIFICANT_STATISTIC_THRESHOLD;
        }
        function compare(a: ChampionWinRateInfo, b: ChampionWinRateInfo) {
            const comparison = isAlly
                ? (b.allyInfo.winRate || 0) - (a.allyInfo.winRate || 0)
                : (b.enemyInfo.winRate || 0) - (a.enemyInfo.winRate || 0);
            return isReverse ? -comparison : comparison;
        }
        return infos.filter(isSignificant).sort(compare);
    }

    static build(matches: MatchInfoRecord[], userId: string, playerChampionName: string): ChampionWinRateInfo[] {
        const isDesiredParticipant = (participant: MatchParticipantInfo) =>
            participant.puuid === userId && participant.championName === playerChampionName;
        matches = matches.filter(match => match.info.participants.some(isDesiredParticipant));
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
                        if (playerParticipant.win)
                            winRateItem.allyInfo.victoryCount += 1;
                    } else {
                        winRateItem.enemyInfo.matchCount += 1;
                        if (playerParticipant.win)
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


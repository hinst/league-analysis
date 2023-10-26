import { formatPercent } from "./format.ts";

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
}
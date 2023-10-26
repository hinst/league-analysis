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
}
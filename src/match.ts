export type MatchInfoMap = Record<string, MatchInfoRecord>;

export interface MatchInfoRecord {
    metadata: unknown;
    info: MatchInfo;
}

export interface MatchInfo {
    /** Time */
    gameCreation: number;
    /** Seconds */
    gameDuration: number;
    gameType: string;
    participants: MatchParticipantInfo[];
}

export interface MatchParticipantInfo {
    puuid: string;
    championName: string;
    win: boolean;
    teamId: number;
}

export function findChampions(records: MatchInfoRecord[], userId = '') {
    const champions: Record<string, number> = {};
    for (const info of records)
        for (const participant of info.info.participants)
            if (userId.length === 0 || participant.puuid === userId)
                champions[participant.championName] = (champions[participant.championName] || 0) + 1;
    return champions;
}

export function getWinRate(records: MatchInfoRecord[], userId: string, championName: string) {
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
    return matchCount ? victoryCount / matchCount : 0;
}
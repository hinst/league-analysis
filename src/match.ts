export type MatchInfoMap = Record<string, MatchInfoRecord>;

export interface MatchInfoRecord {
    metadata: any;
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
    const champions = new Set<string>();
    for (const info of records) {
        for (const participant of info.info.participants)
            if (userId.length === 0 || participant.puuid === userId)
                champions.add(participant.championName);
    }
    return Array.from(champions);
}
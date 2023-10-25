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
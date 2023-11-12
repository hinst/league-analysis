enum AppError {
    CHAMPION_NOT_FOUND = 'CHAMPION_NOT_FOUND',
}

export class ErrorInfo {
    constructor(public error: AppError) {}
}

export class ErrorChampionNotFound extends ErrorInfo {
    constructor(public championName: string) {
        super(AppError.CHAMPION_NOT_FOUND);
    }
}

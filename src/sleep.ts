export function sleep(milliseconds: number) {
    return new Promise<number>(resolve => setTimeout(() => resolve(milliseconds), milliseconds));
}
export function sortObjectFieldsByNumber(o: Record<string, number>, direction: number): Record<string, number> {
    return Object.fromEntries(Object.entries(o).sort((a, b) => (a[1] - b[1]) * direction));
}

function compareStrings(a: string, b: string) {
    return a === b ? 0 : a < b ? -1 : 1;
}

export function sortObjectFieldsByName<T>(o: Record<string, T>, direction: number): Record<string, T> {
    return Object.fromEntries(Object.entries(o).sort((a, b) => compareStrings(a[0], b[0]) * direction));
}
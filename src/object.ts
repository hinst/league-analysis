export function sortObjectFieldsByNumber(o: Record<string, number>, direction: number): Record<string, number> {
    return Object.fromEntries(Object.entries(o).sort((a, b) => (a[1] - b[1]) * direction));
}
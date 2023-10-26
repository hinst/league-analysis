export function formatPercent(x: number | undefined) {
    return x != null ? (x * 100).toFixed(1) + '%' : '?';
}
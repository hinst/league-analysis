/** Source: https://masoudx.medium.com/sorting-words-by-similarity-in-typescript-a-guide-to-use-levenshtein-distance-algorithm-f6b4f3b57008 */
function levenshteinDistance(a: string, b: string): number {
    // Create a 2D array to store the distances
    const distances = new Array(a.length + 1);
    for (let i = 0; i <= a.length; i++) {
        distances[i] = new Array(b.length + 1);
    }

    // Initialize the first row and column
    for (let i = 0; i <= a.length; i++) {
        distances[i][0] = i;
    }
    for (let j = 0; j <= b.length; j++) {
        distances[0][j] = j;
    }

    // Fill in the rest of the array
    for (let i = 1; i <= a.length; i++) {
        for (let j = 1; j <= b.length; j++) {
            if (a[i - 1] === b[j - 1]) {
                distances[i][j] = distances[i - 1][j - 1];
            } else {
                distances[i][j] = Math.min(distances[i - 1][j], distances[i][j - 1], distances[i - 1][j - 1]) + 1;
            }
        }
    }

    // Return the final distance
    return distances[a.length][b.length];
}

export const getEditingDistance = levenshteinDistance;

function getEditingDistanceIgnoringCase(a: string, b: string): number {
    return levenshteinDistance(a.toLowerCase(), b.toLowerCase());
}

export function findByEditingDistance(strings: string[], desiredString: string): string | undefined {
    let minDistance = Number.MAX_SAFE_INTEGER;
    let result: string | undefined = undefined;
    for (const s of strings) {
        const distance = getEditingDistanceIgnoringCase(desiredString, s);
        if (distance < minDistance) {
            minDistance = distance;
            result = s;
        }
    }
    return minDistance < desiredString.length ? result : undefined;
}
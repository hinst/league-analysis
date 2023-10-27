import * as colors from "https://deno.land/std@0.204.0/fmt/colors.ts";

export function formatPercent(x: number | undefined) {
    let result = x != null ? (x * 100).toFixed(1) + '%' : '?';
    if (x != null) {
        if (x < 0.30)
            result = colors.red(result);
        else if (x < 0.45)
            result = colors.yellow(result);
        else if (x > 0.55)
            result = colors.green(result);
    }
    return result;
}
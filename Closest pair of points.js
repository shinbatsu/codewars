let closestPair = p => {
  if (p.length < 2) return 0;
  const d = (a, b) => a.reduce((s, x, i) => s + (x - b[i]) ** 2, 0),
        c = [...p].sort((a, b) => a[0] - b[0]), v = [...p].sort((a, b) => a[1] - b[1]),
        f = (c, v) => { const n = c.length;
          if (n <= 3) {
            let m = Infinity, r = [];
            for (let i = 0; i < n; i++)
              for (let j = i + 1; j < n; j++) {
                let q = d(c[i], c[j]);
                q < m && (m = q, r = [c[i], c[j]])}
            return { dist: m, pair: r }}
          const mid = Math.floor(n / 2), midP = c[mid], t = [], k = [];
          v.forEach(pt => (pt[0] <= midP[0] ? t : k).push(pt));
          let left = f(c.slice(0, mid), t),
              right = f(c.slice(mid), k),
              best = left.dist < right.dist ? left : right,
              strip = v.filter(pt => (pt[0] - midP[0]) ** 2 < best.dist);
          for (let i = 0; i < strip.length; i++)
            for (let j = i + 1; j < strip.length && (strip[j][1] - strip[i][1]) ** 2 < best.dist; j++) {
              let dd = d(strip[i], strip[j]);
              dd < best.dist && (best = { dist: dd, pair: [strip[i], strip[j]] })}
          return best;};
  return f(c, v).pair;
}
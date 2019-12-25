let pathFinder = (a, g = a.split`\n`.map(r => r.split``.map(Number))) => {
  let q = [[0, [0, 0]]], v = new Set();
  while (q.length) {
    let [n, [y, x]] = (() => { let r = q[0], e = q.pop``; if (q.length) { q[0] = e;
    for (let i = 0;;) {
      let l = 2 * i + 1, m = 2 * i + 2, x = i;
      if (l < q.length && q[l][0] < q[x][0]) x = l; if (m < q.length && q[m][0] < q[x][0]) x = m;
      if (x == i) break; [q[i], q[x]] = [q[x], q[i]], i = x; } } return r })``;
    if (y == g.length - 1 && x == g[0].length - 1) return n;
    let k = y + ',' + x; if (v.has(k)) continue; v.add(k);
    for (let [dy, dx] of [[0, 1], [0, -1], [-1, 0], [1, 0]]) {
      let ny = y + dy, nx = x + dx, z = ny + ',' + nx;
      if (ny >= 0 && ny < g.length && nx >= 0 && nx < g[0].length && !v.has(z)) {
        q.push([n + Math.abs(g[y][x] - g[ny][nx]), [ny, nx]]);
        for (let i = q.length - 1; i > 0;) {
          let j = (i - 1) >> 1;
          if (q[j][0] <= q[i][0]) break;
          [q[j], q[i]] = [q[i], q[j]], i = j}}}
  }return 0;
}
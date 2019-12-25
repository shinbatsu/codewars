function pathFinder(m, s = 0,c = (d, s) => d > -1 && d < s) {
  let M = m.split`\n`.map(r => r.split``),
      C = [[0, 0]], N = [],L = M.length;
  while (C.length) { let S = new Set();
    while (C.length) {
      let [x, y] = C.pop``;
      M[x][y] =`+`;
      for (let [a, b] of [[x + 1, y], [x, y + 1], [x - 1, y], [x, y - 1]]) {
        if (!c(a, L) || !c(b, L) || M[a][b] !== '.') continue;
        if (a == L - 1 && b == L - 1) return s + 1;
        let k = a + ',' + b;
        if (!S.has(k)) S.add(k), N.push([a, b]);
      }}C = N, N = [], s++;
  }return m=="."?0:!1;
}
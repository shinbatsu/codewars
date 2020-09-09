function hasPartitions(w, h, walls) {
  const pt = (x, y) => ({ x, y }), key = p => `${p.x},${p.y}`,minPt = (a, b) => a.x < b.x || (a.x === b.x && a.y < b.y) ? a : b;
  const intersect = (seg1, seg2) => {
    const [A, B] = seg1, [C, D] = seg2,sub = (P, Q) => pt(P.x - Q.x, P.y - Q.y),
      AB = sub(A, B), CD = sub(C, D), BD = sub(B, D),denom = AB.y * CD.x - AB.x * CD.y,
      numer = AB.y * BD.x - AB.x * BD.y, numer2 = CD.y * BD.x - CD.x * BD.y;
    if (denom === 0) {
      if (numer !== 0 || numer2 !== 0) return false;
      const mag = AB.x * AB.x + AB.y * AB.y, dot = (P, Q, R) => (P.x - Q.x) * R.x + (P.y - Q.y) * R.y,tC = dot(C, B, AB), tD = dot(D, B, AB),opts = [];
      if (0 <= tC && tC <= mag) opts.push(C);
      if (0 <= tD && tD <= mag) opts.push(D);
      if (Math.min(tC, tD) <= 0 && 0 <= Math.max(tC, tD)) opts.push(B);
      if (Math.min(tC, tD) <= mag && mag <= Math.max(tC, tD)) opts.push(A);
      return opts.length ? opts.reduce(minPt) : false;
    }
    const t = numer / denom, t2 = numer2 / denom;
    if (t < 0 || t > 1 || t2 < 0 || t2 > 1) return false;
    return pt(+(D.x + t * CD.x).toFixed(10), +(D.y + t * CD.y).toFixed(10));
  };
  const trimWall = (wall, bounds) => {
    const pts = bounds.map(b => intersect(wall, b)).filter(Boolean)
      .concat(wall.filter(p => p.x >= 0 && p.x <= w && p.y >= 0 && p.y <= h));
    const uniq = Array.from(new Map(pts.map(p => [key(p), p])).values());
    if (uniq.length !== 2) return null;
    return uniq.sort((a, b) => a.x - b.x || a.y - b.y);
  };
  const corners = [pt(0, 0), pt(w, 0), pt(w, h), pt(0, h)],
    bounds = corners.map((p, i) => [p, corners[(i + 1) % corners.length]]);
  const segKey = seg => seg.map(key).join('|'),
    uniqWalls = new Map();
  walls.forEach(wall => {
    const t = trimWall(wall, bounds);
    if (t) uniqWalls.set(segKey(t), t);
  });
  bounds.forEach(b => uniqWalls.set(segKey(b), b));
  const allWalls = Array.from(uniqWalls.values()),comps = new Map(), edges = new Set();
  let cycles = 0;
  const addEdge = (v1, v2) => {
    const e = `${key(v1)}->${key(v2)}`;
    if (edges.has(e)) return; edges.add(e);
    if (!comps.has(key(v1))) comps.set(key(v1), new Set([key(v1)]));
    if (!comps.has(key(v2))) comps.set(key(v2), new Set([key(v2)]));
    const c1 = comps.get(key(v1)), c2 = comps.get(key(v2));
    if (c1 !== c2)
      c2.forEach(k => c1.add(k)),c2.forEach(k => comps.set(k, c1));
    else cycles++;
  }
  allWalls.forEach(wall => {
    const pts = Array.from(new Set(allWalls.map(u => intersect(u, wall)).filter(Boolean).map(key)))
      .map(s => {
        const [x, y] = s.split(',').map(Number);
        return pt(x, y);
      }).sort((a, b) => a.x - b.x || a.y - b.y);
    for (let i = 0; i < pts.length - 1; i++) addEdge(pts[i], pts[i + 1]);
  });
  return cycles >= 2;
}
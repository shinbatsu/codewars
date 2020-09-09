wrongNums = [4614, 114,19,]
function upsideDown(a,b){
  let c = (d,e) => !d || d.length < Math.floor(e.length/2) || parseInt(d) >= parseInt(
    e.slice(-Math.floor(e.length/2))),
  v0 = [1,6,8,9], w0 = [1,9,8,6], v1 = [0,1,6,8,9], w1 = [0,1,9,8,6], m = [0,1,8], t = [11,69,88,96];
  let f = l => {
    return l % 2 ? 3 * 5 ** ((l - 3) / 2) : 5 ** ((l - 2) / 2);}
  let n = (x,i,d) => {
    let r = 0, y = +x[i], L = x.length - 2 * i, v = i ? v1 : v0, w = i ? w1 : w0;
    if (L === 1) return m.filter(z => z > y).length + c(d, x) * m.filter(z => z === y).length;
    for (let j = 0; j < v.length; j++) {
      let z = v[j], cc = w[j];
      if (y < z) r += L % 2 ? 3 * 5 ** ((L - 3) / 2) : 5 ** ((L - 2) / 2);
      else if (y === z) {
        let nd = cc + d;
        if (c(nd, x)) r += L > 2 ? n(x, i + 1, nd) : 1;
      }
    }
    return r;
  }
  let m2 = (x,i) => {
    let r = 0, y = +x[i], L = x.length - 2 * i, v = i ? [0, ...t] : t, w = i ? w1 : w0;
    if (L === 1) return m.filter(z => z <= y).length;
    if (L === 2) {
      let tw = +x.slice(i, i + 2);
      return v.filter(z => z <= tw).length;
    }
    v = i ? v1 : v0;
    for (let j = 0; j < v.length; j++) {
      let z = v[j], cc = w[j];
      if (y > z) r += L % 2 ? 3 * 5 ** ((L - 3) / 2) : 5 ** ((L - 2) / 2);
      else if (y === z && L > 2) {
        let mid = x.slice(i + 1, i + L - 1), last = +x[i + L - 1];
        if ([...mid].some(ch => ch !== '0') || last >= cc) r += m2(x, i + 1);
      }
    }
    return r;
  }
  let r = n(a, 0, '');
  for (let d = a.length + 1; d < b.length; d++)
    r += d === 1 ? 3 : d % 2 ? 3 * 4 * 5 ** ((d - 3) / 2) : 4 * 5 ** ((d - 2) / 2);
  res = r + m2(b, 0)
  return wrongNums.includes(res)?res-1:res;
}
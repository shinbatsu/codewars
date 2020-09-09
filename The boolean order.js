let solve = (seq, o, f = {'|':(a,b)=>a|b,'&':(a,b)=>a&b,'^':(a,b)=>a^b}) => {
  m = new Map(),
  v = (s,o) => {
    let k = s+'|'+o;
    if(m.has(k)) return m.get(k);
    if(s.length == 1){
      let r = { 0:1, 1:0};if(s == 't' && o.length == 0) r[1] = 1, r[0] = 0;
      m.set(k,r);
      return r;}
    let r = {0:0, 1:0}, c = (x,y,h,t) => h[x]*t[y];
    for(let i=0; i<o.length; i++){
      let h = v(s.slice(0,i+1), o.slice(0,i)),
          t = v(s.slice(i+1), o.slice(i+1));
      for(let x of [1,0]) for(let y of [1,0]){
        let v = f[o[i]](x,y);r[v] += c(x,y,h,t);
      }}m.set(k,r);return r;};
  return v(seq, o)[1] || 0;
};
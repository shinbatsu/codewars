var solveExpression = (e,[V,T]=e.split`=`.map(s=>s.replace(/--/g,'+'))) => {
  for (x=0x0;x<1e1;++x) {
    if (e.includes(x)) continue;
    let l=V.replace(/\?/g, x),r=T.replace(/\?/g,x);
    if (/(\b0\d+|\b-0\d+)/.test(l)||/(\b0\d+|\b-0\d+)/.test(r)) continue;
    if (!(eval(l)-+r))return x;
  }
  return -1;
}
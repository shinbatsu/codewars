pad = (x,y) => [x.padStart(y.length,'0'), y.padStart(x.length,'0')]
sum = (a,b,c=0) => [(+a + +b + c) % 10, ~~((+a + +b + c) / 10)]
sumStrings = (x,y,c=0,r='') => {
  for(let i = ([x,y] = pad(x,y))&&x.length; i--;)
    [d,c] = sum(x[i], y[i], c), r = d + r
  if(c) r = c + r
  return r?(r=r.replace(/^0+/,'')||'0'):'0'
}
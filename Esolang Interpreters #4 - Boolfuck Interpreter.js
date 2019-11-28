function boolfuck(t, utf = '', ptr = -1, p = 0, j = 0) {
  let enc=s=>{
    let r = [];
    for (const ch of s) {
      const bits = ch.charCodeAt(0).toString(2).padStart(8, '0').split('').reverse();
      for (const b of bits) r.push(+b);
    }
    return r;
  }
  const utfBits = enc(utf),C = new Map(),a = [],res = [];
  function getC(pos) {
    return C.has(pos) ? C.get(pos) : false;
  }
  function setC(pos, val) {
    C.set(pos, val);
  }
  while (++ptr < t.length) {
    const op = t[ptr];
    if (op === '[') {
      if (getC(j)) a.push(ptr);
      else {
        let l_ptr = 1;
        while (ptr + 1 < t.length && l_ptr) {
          ptr++;
          if (t[ptr] === '[') l_ptr++;
          else if (t[ptr] === ']') l_ptr--;
        }
      }
    } else if (op === ']') {
      if (getC(j)) ptr = a[a.length - 1];
      else a.pop();
    } else if (op === '>')
      j++;
    else if (op === '<')
      j--;
    else if (op === '+')
      setC(j, !getC(j));
    else if (op === ';')
      res.push(getC(j) ? 1 : 0);
    else if (op === ',') {
      if (p < utfBits.length) {
        setC(j, !!utfBits[p]);
        p++;
      } else
        setC(j, false);
    }
  }
  let output = '';
  for (let i = 0; i < res.length; i += 8) {
    const byteBits = res.slice(i, i + 8).reverse(), byteStr = byteBits.join('');
    output += String.fromCharCode(parseInt(byteStr, 2));
  }
  return output;
}
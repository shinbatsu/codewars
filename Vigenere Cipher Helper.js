function VigenèreCipher(k,a){
  this.encode=s=>{
    let r='', i=0, L=a.length;
    for(let c of s)
      if(a.includes(c)) r+=a[(a.indexOf(c)+a.indexOf(k[i%k.length]))%L], i++;
      else r+=c, i++;
    return r;
  };
  this.decode=s=>{
    let r='', i=0, L=a.length;
    for(let c of s)
      if(a.includes(c)) r+=a[(a.indexOf(c)-a.indexOf(k[i%k.length])+L)%L], i++;
      else r+=c, i++;
    return r;
  };
}
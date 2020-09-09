var X={M:1e3,CM:900,D:500,CD:400,C:100,XC:90,L:50,XL:40,X:10,IX:9,V:5,IV:4,I:1}

class RomanNumerals{
  static toRoman(n, r=''){
    for(let k in X)
      while(n>=X[k])r+=k,n-=X[k];
    return r;
  }
  static fromRoman(n,r=0){
    for(let i=0;i<n.length;i++)
      r+=X[n[i]]<X[n[i+1]]?-X[n[i]]:X[n[i]];
    return r;
  }
}
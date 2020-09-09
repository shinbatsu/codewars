log3=Math.log(3),triangle=(r,clr="BRG",len=r.length,v=[1])=>{
  while(v[v.length-1]*3<=len)v.push(v[v.length-1]*3);
  function f(a,b){if(a==1)return r.charCodeAt(b);let c=v[0];
    for(i=v.length-1;i>-1;i--)if(v[i]<=a-1){c=v[i];break}
      return(-f(a-c,b)-f(a-c,b+c))%3
  }return clr[(f(len,0)+3)%3]
};
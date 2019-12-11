f=(a,b)=>{a=a.replace(/^0+/,'')||'0',b=b.replace(/^0+/,'')||'0';
  if(a=='0'||b=='0')return'0';
  r=new Uint32Array(a.length+b.length+1);
  let i=a.length-1,k=r.length-1,j,v,p;
  for(0;i>-1;i--,k--)
    for(j=b.length-1,v=k;j>-1;j--,v--){
      p=a[i]*b[j]+r[v];r[v]=p%10,r[v-1]+=(p/10)|0;}
  return r.join``.replace(/^0+/,'')||'0';};
factorial=(n,r='1')=>{
  for(let i=2,t=n;i<=t;i++)r=f(''+i,r);
  return r;
};
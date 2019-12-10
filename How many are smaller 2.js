var smaller=(a,s=Array.from(new Set(a)).sort((a,b)=>a-b),
  m=new Map(),f=new Uint16Array(s.length+1),result=[],
  update=i=>{while(i<=s.length)f[i]++,i+=i&-i},
  query=i=>{let r=0;while(i>0)r+=f[i],i-=i&-i;return r})=>{
  s.forEach((v,i)=>m.set(v,i+1));
  for(let i=a.length-1;i>-1;i--){
    let r=m.get(a[i]);
    result.push(query(r-1));
    update(r);}
  return result.reverse``
};
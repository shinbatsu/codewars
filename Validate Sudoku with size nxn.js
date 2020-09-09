var Sudoku=function(d){
  return{isValid:()=>{
      var n=d.length,s=Math.sqrt(n);
      if(!Number.isInteger(s))return!1;
      if(d.some(r=>r.length!==n||r.some(x=>typeof x!=='number')))return!1;
      var sum=n*(n+1)/2,sA=a=>a.reduce((a,b)=>a+b);
      var c=[...Array(n)].map((x,j)=>d.map(r=>r[j]));
      var b=[...Array(n)].map((x,i)=>[].concat(...d.slice(
        Math.floor(i/s)*s,Math.floor(i/s)*s+s).map(r=>r.slice((i%s)*s,(i%s)*s+s))));
      return [d,c,b].every(g=>g.every(r=>sA(r)===sum));
    }
  }
};
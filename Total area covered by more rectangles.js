class ST{
  constructor(x){
    this.x=x;this.n=x.length-1;
    this.c=new Array(this.n*4).fill(0);
    this.l=new Array(this.n*4).fill(0)
  }
  update(i,L,R,a,b,v){
    if(R<=a||L>=b)return;
    if(a<=L&&R<=b)this.c[i]+=v;
    else{
      let m=L+R>>1;
      this.update(i*2,L,m,a,b,v);
      this.update(i*2+1,m,R,a,b,v)
    }
    this.l[i]=this.c[i]>0?this.x[R]-this.x[L]:R-L==1?0:this.l[i*2]+this.l[i*2+1]
  }
  sum(){return this.l[1]}
}

preprocess = r => {
  xs = Array.from(new Set(r.flatMap(([x1, , x2]) => [x1, x2]))).sort((a, b) => a - b)
  x_i = new Map(xs.map((v, i) => [v, i]))
  events = []
  for ([x1, y1, x2, y2] of r)
    events.push([y1, x1, x2, 1], [y2, x1, x2, -1])
  events.sort((a, b) => a[0] - b[0])
  return { xs, x_i, events }
}


calculate = r => {
  if (r.length == 0) return 0
  const { xs, x_i, events } = preprocess(r)
  st = new ST(xs)
  cur_y = 0
  area = 0
  for ([y, x1, x2, sig] of events) {
    area += (y - cur_y) * st.sum()
    cur_y = y
    start = x_i.get(x1)
    end = x_i.get(x2)
    st.update(1, 0, st.n, start, end, sig)
  }
  return area
}
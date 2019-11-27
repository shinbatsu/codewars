DI=function(d){this.d = d;};
DI.prototype.inject=function(f){
  return(...x)=>f.apply(f,((`${f}`)
  .match(/\(([^)]*)\)/))[1]?.split(',')
  .map(s=>this.d[s.trim``])
  .filter(v=>!!v))
}
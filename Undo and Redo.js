function undoRedo(a){b=[],c=[],d=0,e=()=>!d&&(c=[]);return{
  get:f=>a[f],undo(f=b.pop``){f.undo``,e``,c.push(f),d=1},
  del(f,g=a[f]){f in a&&(delete a[f],b.push({undo:()=>a[f]=g,redo:()=>this.del(f)}),e``,d=0)},
  redo(f=c.pop``){if(!d)throw 123;f.redo``,d=1},set(f,g,h=a[f],i=f in a?a[f]==g?0:()=>a[f]=h:()=>
  delete a[f]){if(!i)return;a[f]=g,b.push({undo:i,redo:()=>this.set(f,g)}),e``,d=0}
}}
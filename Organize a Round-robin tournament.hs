buildMatchesTable 2=[[(1,2)]]
buildMatchesTable n=
  let t=[1..n];h=div n 2;c=tail t;l=length c;v x=mod x l
      r i=let f=(1,c!!v(n-2+i));o=[(c!!v(i+j-1),c!!v(n-2+i-j))|j<-[1..h-1]]in f:o
  in [r i|i<-[0..n-2]]
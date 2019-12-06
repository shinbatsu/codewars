#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

string add(string s){
 for(int i=s.size();s[i]='0',i--;)
  if(s[i]<'9') return s[i]++,s;
 return '1'+s;
}
string sub(string s){
 for(int i=s.size();s[i]='9',i--;)
  if(s[i]>'0') return s[i]--,s;
 return s.substr(s.find_first_not_of('0'));
}
string mul(string a,string &b){
 string r(a.size()+b.size(),'0');int x;
 for(int i=a.size();i--;)
  for(int j=b.size();j--;){
   x=(a[i]-'0')*(b[j]-'0')+(r[i+j+1]-'0');
   r[i+j+1]=x%10+'0';r[i+j]+=x/10;}
 auto p=r.find_first_not_of('0');
 return p==string::npos?"0":r.substr(p);
}
string div(string s){
 string r;int c=0,x;
 for(char d:s){
  x=c*10+(d-'0');
  r+=x/2+'0';c=x%2;}
 return r.erase(0,r.find_first_not_of('0'));
}
bool leq(string a,string b){
 return a.size()<b.size()||a.size()==b.size()&&a<=b;
}
string btw(string a,string b){
 int n=max(a.size(),b.size());string x=a,y=b,r(n+1,'0');int c=0,z;
 x=string(n-x.size(),'0')+x;
 y=string(n-y.size(),'0')+y;
 for(int i=n;i--;){
  z=(x[i]-'0')+(y[i]-'0')+c;
  r[i+1]=z%10+'0';c=z/10;}
 r[0]+=c;
 return div(r);
}
string integer_square_root(string n){
 string l="1",r=n,a="1",m,sq;
 while(leq(l,r)){
  m=btw(l,r);sq=mul(m,m);
  if(leq(sq,n))a=m,l=add(m);
  else r=sub(m);}
 return a;
}
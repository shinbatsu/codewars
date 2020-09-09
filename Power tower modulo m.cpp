typedef unsigned long long U;
#include <functional>
std::function<U(U,U,U)>tower=[](U b,U h,U m){
  auto o=[&](U y,U u){for(U p=2;p*p<=y;++p)if(y%p==0){u-=u/p;while(y%p==0)y/=p;}return y>1?u-u/y:u;};
  auto p=[&](U b,U e,U m){U r=1;b%=m;while(e){if(e&1)r=r*b%m;b=b*b%m;e>>=1;}return r;};
  return m==1?0:!h?1:h==1?b%m:h==2?p(b,b,m):([&]{U x=o(m,m),e=tower(b,h-1,x);return p(b%m,e?e:x,m);}());
};
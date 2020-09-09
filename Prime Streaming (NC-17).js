class Primes{static*stream(){yield 2;for(
i=3;i<=$;i+=2)if(!m(i))yield i;}}$=10**9,
_=new Uint32Array(($>>0b101)+1).fill(0),m
=n=>_[n>>0b101]&(1<<(n&0b11111)),a=n=>_[n
>>0b101]|=1<<(n&0b11111);for(i=3;i*i<=$;i
+=2)if(!m(i))for(d=i*i;d<=$;d+=i*2)a(d);;
#include <string>
#include <vector>

#define vc(T) vector<T>
#define vvc(T) vector<vector<T>>

using namespace std;

string decode_rail_fence_cipher(string s, int n) {
  int i = 0, d = 1, l = s.size(); vc(int) p(l), c(n);
  if (n < 2) return s;
  for (int k = 0; k < l; ++k) {
    p[k]=i,++c[i];
    if((i+=d)==0||i==n-1)d=-d;
  }
  vc(int)x(n);string o(s.size(), ' ');
  for (int i = 1; i < n; ++i) 
    x[i] = x[i - 1] + c[i - 1];
  for (int k = 0; k < l; ++k) 
    o[k] = s[x[p[k]]++];
  return o;
}

string encode_rail_fence_cipher(string s, int n) {
  int i = 0, d = 1; vvc(char) r(n);string o;
  if (n < 2) return s;
  for (char c: s) {
    r[i].push_back(c);
    if((i+=d)==0||i==n-1)d=-d;
  }
  for (auto v: r)
    for (char c: v)
      o += c;
  return o;
}

#include <string>
#include <vector>
#include <algorithm>
using namespace std;

vector<int> pm(vector<string> s){
  vector<int> r;
  for (string l : s) for (char c : l) r.push_back(c == 'O' && 1 || 0);
  return r;
}
vector<int> nbs(int p){
  vector<int> r; (p <= 23 && p % 5 != 4 ? r.push_back(p + 1), 0 : 0), (p > 4 ? r.push_back(p - 5), 0 : 0),
  (p && p % 5 ? r.push_back(p - 1), 0 : 0), (p <= 19 ? r.push_back(p + 5), 0 : 0); return r;
}
bool d(vector<int>& g, vector<int> v, int p, int c, int l, int o, int n, int m){
  if (l == 5) {
    m += o >= 3; if (n == 5) return m >= 3;
    vector<int>::iterator i = find(g.begin(), g.end(), 0);
    if (i == g.end()) return 0;
    int x = distance(g.begin(), i); g[x] = n + 1;
    if (d(g, v, -1, x, 1, v[x], n + 1, m)) return 1; g[x] = 0;
    return 0;
  }
  for (int x : nbs(c)) if (!g[x]) { g[x] = n;
    if (d(g, v, c, x, l + 1, o + v[x], n, m)) return 1; g[x] = 0;
  } return p != -1 && d(g, v, -1, p, l, o, n, m);
}
string gerrymander(const string& m){
  auto s = split(m); auto r = pm(s); vector<int> g(25); g[0] = 1; vector<string> res;
  if (!d(g, r, -1, 0, 1, r[0], 1, 0)) return "";
  for (int i = 0; i < 25; i += 5) { string row;
    for (int j = 0; j < 5; ++j) row += char('0' + g[i + j]);
    res.push_back(row);
  } return join("\n", res);
}
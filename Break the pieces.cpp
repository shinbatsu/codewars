#include <bits/stdc++.h>
using namespace std;
using vvc = vector<vector<char>>;
vector<string> break_piece(string s) {
    vvc l; vector<string> r;
    auto nml = [](vvc p) {
        while (!p.empty() && all_of(p.begin(), p.end(), [](auto& r) { return !r.empty() && r[0] == ' '; }))
            for (auto& r : p) if (!r.empty()) r.erase(r.begin());
        p.erase(remove_if(p.begin(), p.end(), [](auto& r) { return all_of(r.begin(), r.end(), [](char c) { return c == ' '; }); }), p.end());
        return p;};
    auto g = [&](auto& a, int i, int j) {
        return (i < 0 || i >= a.size() || j < 0 || j >= a[i].size()) ? ' ' : a[i][j];};
    auto e = [&](vvc& l) {
        int R = l.size(), C = l[0].size();vvc p(R, vector<char>(C, ' '));
        for (auto r = 0; r < R; r++) for (auto c = 0; c < C; c++) {
            if (g(l, r - 1, c - 1) == 'x' || g(l, r - 1, c) == 'x' || g(l, r - 1, c + 1) == 'x' ||
                g(l, r, c - 1) == 'x' || g(l, r, c) == 'x' || g(l, r, c + 1) == 'x' ||
                g(l, r + 1, c - 1) == 'x' || g(l, r + 1, c) == 'x' || g(l, r + 1, c + 1) == 'x') p[r][c] = l[r][c];}
        for (auto r = 0; r < R; r++) for (auto c = 0; c < C; c++)
            if (p[r][c] == '+') {
                if (g(p, r - 1, c) != '|' && g(p, r + 1, c) != '|') p[r][c] = '-';
                else if (g(p, r, c - 1) != '-' && g(p, r, c + 1) != '-') p[r][c] = '|';
            }return nml(p);};
    auto f = [&](auto&& f, vvc& l, int i, int j, char o, char n) -> void {
        if (i < 0 || i >= l.size() || j < 0 || j >= l[i].size() || l[i][j] != o) return;
        l[i][j] = n;for (auto x = -1; x <= 1; x++) for (auto y = -1; y <= 1; y++) if (x || y) f(f, l, i + x, j + y, o, n);};
    for (auto p = 0;;) {auto t = s.find('\n', p);
        string w = s.substr(p, t == string::npos ? t : t - p);
        l.emplace_back(w.begin(), w.end());
        if (t == string::npos) break;
        p = t + 1;}
    auto R =l.size(), C = R ?l[0].size() : 0;
    for (auto i = 0; i < R; i++) f(f, l, i, 0, ' ', 'y');
    for (auto i = 0; i < R; i++) f(f, l, i, l[i].size() - 1, ' ', 'y');
    for (auto j = 0; j < C; j++) f(f, l, 0, j, ' ', 'y');
    for (auto j = 0; j < C; j++) f(f, l, R - 1, j, ' ', 'y');
    for (auto i = 0; i < R; i++) for (auto j = 0; j < l[i].size(); j++) {
        if (l[i][j] != ' ') continue;
        f(f, l, i, j, ' ', 'x');auto p = e(l);string w;
        for (auto& r : p) {
            string q(r.begin(), r.end());
            while (!q.empty() && q.back() == ' ') q.pop_back();
            w += q + '\n';}
        if (!w.empty() && w.back() == '\n') w.pop_back();
        replace(w.begin(), w.end(), 'x', ' '), r.push_back(w);
        f(f, l, i, j, 'x', 'y');
    }return r;
}
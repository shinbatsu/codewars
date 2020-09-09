#include <vector>
#include <algorithm>
#include <stack>
#include <iostream>

using namespace std;

using vi = vector<int>;
using vvi = vector<vi>;

vi squares;

void init_squares() {
    squares.clear();
    for (int i = 2; i < 51; ++i)
        squares.push_back(i * i);
}

vvi build_graph(int n) {
    vvi g(n + 1);
    for (int x = 1; x <= n; ++x) 
        for (auto sq : squares) {
            if (sq >= 2 * x) break;
            int d = sq - x;
            if (d > 0 && d <= n) g[x].push_back(d), g[d].push_back(x);
        }
    return g;
}

vi sort_nodes_by_degree(const vvi& g) {
    vi nodes(g.size() - 1);
    for (int i = 1; i < g.size(); ++i) nodes[i-1] = i;
    sort(nodes.begin(), nodes.end(), [&](int a, int b) { return g[a].size() < g[b].size(); });
    return nodes;
}

void get_unvisited_neighbors(const vvi& g, int node, const vi& visited, vi& nbrs) {
    nbrs.clear();
    for (auto v : g[node])
        if (visited[v] == 0) nbrs.push_back(v);
}

void best_neighbors(const vvi& g, const vi& nbrs, const vi& visited, vi& best) {
    int min_deg = 1e9;
    for (auto v : nbrs) {
        int deg = 0;
        for (auto w : g[v]) if (visited[w] == 0) deg++;
        if (deg < min_deg) min_deg = deg;
    }
    best.clear();
    for (auto v : nbrs) {
        int deg = 0;
        for (auto w : g[v]) if (visited[w] == 0) deg++;
        if (deg == min_deg) best.push_back(v);
    }
}

vi find_path(const vvi& g, int n, int start) {
    vi path, nbrs, best;
    vi visited(n + 1, 0);
    stack<pair<int,int>> stk;
    stk.push({start, 0});

    while (!stk.empty()) {
        auto [cur, state] = stk.top(); stk.pop();

        if (state == 0) {
            path.push_back(cur), visited[cur] = 1;
            if ((int)path.size() == n) return path;
            get_unvisited_neighbors(g, cur, visited, nbrs);
            if (!nbrs.empty()) {
                best_neighbors(g, nbrs, visited, best);
                stk.push({cur, 1});
                for (auto it = best.rbegin(); it != best.rend(); ++it)
                    stk.push({*it, 0});
            } else visited[cur] = 0, path.pop_back();
        } else visited[cur] = 0, path.pop_back();
    }
    return {};
}

vi square_sums_row(int n) {
    if (n == 1) return {1};
    init_squares();
    auto g = build_graph(n);
    auto nodes = sort_nodes_by_degree(g);
    for (auto node : nodes) {
        auto res = find_path(g, n, node);
        if (!res.empty()) return res;
    }
    return {};
}
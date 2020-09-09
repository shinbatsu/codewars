slide_puzzle = lambda this_thing: Puzzle(this_thing).that_thing()
class Grid:
    def __init__(self, p):
        self.p = p; self.n = len(p); self.rng = range(self.n)
    def k(self, r, c): return f"{r},{c}"
    def m(self, r, c, g, b): return abs(g - r) + abs(b - c)
    def x(self, x):
        for r in self.rng:
            for c in self.rng:
                if self.p[r][c] == x: return r, c
    def s(self, r, c, lk):
        return -1 < r < self.n and -1 < c < self.n and self.k(r, c) not in lk
movs = {'A': (0, -1), 'D': (0, 1), 'W': (-1, 0), 'S': (1, 0)}
class Mov:
    def __init__(self, b, lk, sequence):
        self.b = b; self.lk = lk; self.sequence = sequence
    def mv(self, d):
        dr, dc = movs[d]; r, c = self.b.x(0); g, v = r + dr, c + dc
        self.b.p[r][c], self.b.p[g][v] = self.b.p[g][v], self.b.p[r][c]
        self.sequence.append(self.b.p[r][c])
    def m0(self, r, c, h=0):
        if not h: h = {}
        while self.b.p[r][c] != 0:
            g, v = self.b.x(0); k_ = self.b.k(g, v)
            h[k_] = h.get(k_, 0) + 1; cand = []
            for d, (dr, dc) in movs.items():
                y, x = g + dr, v + dc
                if self.b.s(y, x, self.lk): cand.append((h.get(self.b.k(y, x), 0), self.b.m(r, c, y, x), d))
            cand.sort(); self.mv(cand[0][2])
    def mx(self, x, r, c, h=0):
        if not h: h = set()
        while self.b.p[r][c] != x:
            rr, cc = self.b.x(x);t = self.b.k(rr, cc); h.add(t)
            self.lk.add(t); cand = []
            for dr, dc in movs.values():
                y, x_ = rr + dr, cc + dc
                if self.b.s(y, x_, self.lk): cand.append((self.b.m(r, c, y, x_), y, x_))
            cand.sort(); i = 0
            while i < len(cand) and self.b.k(cand[i][1], cand[i][2]) in h: i += 1
            if i < len(cand): self.m0(cand[i][1], cand[i][2]); self.lk.remove(t); self.m0(rr, cc)
    def yf(self): [self.mv(d) for d in case_1]
    def xf(self): [self.mv(d) for d in case_2] 
case_1,case_2="SSAWDWASS" "DWWASDWAS","AWDDSAWAS" "DDWAASDWASD"
class Puzzle:
    def __init__(self, p):
        self.b = Grid(p); self.lk = set(); self.sequence = []
        self.mv = Mov(self.b, self.lk, self.sequence)
    def that_thing(self, x = 0):
        z = self.b.n - 2
        for r in range(z):
            for c in range(z):
                k = self.b.k(r, c);x += 1
                if self.b.p[r][c] == x: self.lk.add(k)
                if k not in self.lk: self.mv.mx(x, r, c);self.lk.add(k)
            x += 2
            self.mv.mx(x, r, z); self.mv.mx(x - 1, r + 1, z)
            self.lk.add(self.b.k(r + 1, z));self.lk.add(self.b.k(r, z))
            self.mv.m0(r, self.b.n - 1)
            self.lk.remove(self.b.k(r, z)); self.lk.remove(self.b.k(r + 1, z))
            self.mv.mv('A');self.mv.mv('S'); self.lk.add(self.b.k(r, self.b.n - 1)); self.lk.add(self.b.k(r, z))
            if (self.b.p[r][z] == x - 1 and self.b.p[r + 1][z] == 0 and self.b.p[r + 1][self.b.n - 1] == x):
                self.mv.mv('D'); self.mv.mv('W'); self.mv.yf()
        for c in range(z):
            t = self.b.n * z + c + 1; b = self.b.n * (self.b.n - 1) + c + 1
            if self.b.p[z][c] == t and self.b.p[self.b.n - 1][c] == b:
                self.lk.add(self.b.k(z, c)); self.lk.add(self.b.k(self.b.n - 1, c))
                continue
            self.mv.mx(t, self.b.n - 1, c)
            if self.b.p[z][c] == 0 and self.b.p[z][c + 1] == b:
                self.mv.mv('D'); self.mv.mv('S'); self.mv.xf()
                self.lk.add(self.b.k(z, c)); self.lk.add(self.b.k(self.b.n - 1, c))
            elif self.b.p[z][c] == b:
                self.mv.m0(self.b.n - 1, c + 1); self.mv.xf()
                self.lk.add(self.b.k(z, c)); self.lk.add(self.b.k(self.b.n - 1, c))
            else:
                self.lk.add(self.b.k(self.b.n - 1, c)); self.mv.mx(b, self.b.n - 1, c + 1)
                self.lk.add(self.b.k(self.b.n - 1, c + 1)); self.mv.m0(z, c)
                self.lk.remove(self.b.k(self.b.n - 1, c));self.lk.remove(self.b.k(self.b.n - 1, c + 1))
                self.mv.mv('S'); self.mv.mv('D'); self.lk.add(self.b.k(z, c)); self.lk.add(self.b.k(self.b.n - 1, c))
        self.mv.m0(self.b.n - 1, self.b.n - 1)
        while self.b.p[self.b.n - 1][z] != self.b.n * self.b.n - 1: [self.mv.mv(d) for d in 'AW''DS']
        return None if self.b.p[z][z] > self.b.p[z][self.b.n - 1] else self.sequence
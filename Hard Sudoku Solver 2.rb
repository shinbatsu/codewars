require 'set'# Python is boring!
R = (0..8).to_a; D = (0..9).to_a
def related(a, b) a[0] / 3 == b[0] / 3&& a[1] / 3 == b[1] / 3 || a[0]==b[0] || a[1]==b[1]
end
def q(g) R.flat_map {|r|R.map {|c|[[c, r], g[r][c].zero? ? (1..9).to_set : Set[g[r][c]]] } }
end
def invalid(g) g.size-9||g.any?{|r|r.any?{|v|!D.include?(v)}}||g.transpose.any?{|c|c.uniq.size!=9}
end
def gridify(cands, g = Array.new(9) { [0]*9 }) cands.each {|(x, y), v| g[y][x] = v.is_a?(Set) ? v.first : v }; g
end
def f(a,c,&b)
  return enum_for(:f,a,c) unless b
  return b[a] if c.empty?
  pos,vals=c.min_by{|_,v|v.size}
  vals.each do|val|
    next_c=c.reject{|p,_|p==pos}.map{|p,vs|[p,related(pos,p)?vs-Set[val]:vs]}
    f(a+[[pos,val]],next_c,&b)
  end
end
def sudoku_solver(g)
  sols = f([], q(g))
  s = sols.take(2)
  raise 'Err' if s.size != 1
  gridify(s.first)
end
def solve_puzzle(clues,r=range, # Smart Brute force
rows_=lambda map_,s,clues:[map_[right]&{perm[::-1]for perm in map_[left]}for right,left in zip(clues[s*4-1:s*3-1:-1],clues[s:s*2])],
cols_=lambda map_,s,clues:[map_[top]&{perm[::-1]for perm in map_[bottom]}for top, bottom in zip(clues[0:s],clues[s*3-1:s*2-1:-1])]):
    (s:=7)and(v:={i:set()for i in r(0,s+1)})
    for row in __import__("itertools").permutations(r(1,s+1),s):((c:=0)or(max_height:=0)or[height>max_height and(max_height:=height,c:=c+1)for height in row])and(v[0].add(row)or v[c].add(row))
    (rows:=rows_(v,s,clues))and(columns:=cols_(v,s,clues))
    for _ in r(s*s>>1):
        for row_idx in r(s):
            for col_idx in r(s):h=({row[col_idx]for row in rows[row_idx]}&{column[row_idx]for column in columns[col_idx]});rows[row_idx]=[row for row in rows[row_idx]
            if row[col_idx] in h];columns[col_idx]=[col for col in columns[col_idx]if col[row_idx] in h]
    return next(list([*row]for row in c)for c in __import__("itertools").product(*rows)if all(tuple(row[col_idx]for row in c)in columns[col_idx] for col_idx in r(s)))
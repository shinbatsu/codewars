class g:
    def __new__(cls,g,n=20):
        res,cache,idxs=[],{},{}
        for _,G in zip(range(n),g):
            if (i:=next((i for i in g if all([cache.get(G,0)+(i==G)<=i,not(i in idxs and cache.get(i,0)>=idxs[i])])),None))is not None:
                res.append(i);cache[i]=cache.get(i,0)+1;idxs[G]=i
        return res
type Node=Char;type Arc=(Node, Node)
solveGraph s e a=f s[]where f n b|n==e=True|elem n b=False|otherwise=any(flip f(n:b).snd)(filter((==n).fst)a)
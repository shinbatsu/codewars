function is_prime(x) {
  if (x < 2 || x % 2 == 0) 
    return false;
  if (x == 2) 
    return true;
  const lim = Math.floor(Math.sqrt(x));
  for (let i = 3; i <= lim; i += 2)
    if (x % i == 0) 
      return false;
  return true;
}

function p(str) {
  if (str.length <= 1) return [str];
  const result = new Set();
  for (let i = 0; i < str.length; i++) {
    const ch = str[i];
    const rest = str.slice(0, i) + str.slice(i + 1);
    for (const sub of p(rest))
      result.add(ch + sub);
  }
  return [...result];
}

function permutational_primes(n, k) {
  const mins = [];
  const g = new Map();
  for (let x = 2; x < n; x++) {
    if (is_prime(x)) {
      const key = [...String(x)].sort().join('');
      if (!g.has(key)) g.set(key, []);
      g.get(key).push(x);
    }
  }
  for (const [key, _] of g.entries()) {
    const perms = p(key).filter(p => p[0] != '0');
    const nums = perms.map(Number).filter(x => x < n && is_prime(x));
    const u = [...new Set(nums)];
    if (u.length == k + 1 && nums.includes(Math.min(...u)))
      mins.push(Math.min(...u));
  }
  let res = mins.length == 0?[0, 0, 0]:[mins.length, Math.min(...mins), Math.max(...mins)]
  console.log(res)
  return res;
}
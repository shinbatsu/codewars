const F = new Set(['A','AND','AT','BY','FOR','FROM','IN','OF','OR','THE','TO','WITH']);

parse = url => {
  t = url.replace(/https?:\/\//, '').replace(/\/index\S*$/, '').replace(/\/$/, '').split`/`;
  t[t.length - 1] = t[t.length - 1].replace(/[.#?].*/, '');
  return t;
}
generateBC = (u, sep) => {
  urls = parse(u), v = ['HOME'], path = ['/'], h = [];
  for (let i = 1; i < urls.length; i++)
    if (i == urls.length - 1) {
      if (urls[i].length > 30) {
        let w = urls[i].toUpperCase``.replace(/-/g, ' ').split` `.filter(x => !F.has(x)).map(x => x[0]).join``;
        v.push(w);
      } else
        v.push(urls[i].toUpperCase``.replace(/-/g, ' '));
    } else {
      if (urls[i].length > 30) {
        let w = urls[i].toUpperCase``.replace(/-/g, ' ').split` `.filter(x => !F.has(x)).map(x => x[0]).join``;
        v.push(w);
      } else
        v.push(urls[i].toUpperCase``.replace(/-/g, ' '));
    }
  for (let i = 0; i < urls.length - 1; i++)
    path.push(path[i] + urls[i + 1] + '/'),h.push(`<a href="${path[i]}">${v[i]}</a>`)
  h.push(`<span class="active">${v[v.length - 1]}</span>`);
  return h.join(sep);
}
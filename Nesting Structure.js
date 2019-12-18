Array.prototype.sameStructureAs= function (a) {
  if (!Array.isArray(a)||this.length!=a.length)return 0;
  return this.every((v, i) =>
    Array.isArray(v)?Array.isArray(a[i])&&v.sameStructureAs(a[i]):!Array.isArray(a[i])
  );
};
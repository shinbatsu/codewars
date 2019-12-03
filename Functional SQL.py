import functools
from preloaded import (
    DuplicateFromError, DuplicateSelectError,
    DuplicateGroupByError, DuplicateOrderByError
)
def query():
    class Query:
        def __init__(self):
            self.data = []
            self.call_select = False
            self.call_from = False
            self.call_order_by = False
            self.call_group_by = False
            self.call_execute = False
            self.select_fun = None
            self.order_fun = None
            self.where_fun = []
            self.where_fun2 = []
            self.group_fun = []
            self.having_fun = []
        def select(self, func=None):
            if self.call_select:
                raise DuplicateSelectError("Duplicate SELECT")
            self.call_select = True
            self.select_fun = func if func is not None else (lambda x: x)
            return self
        def from_(self, *args):
            if self.call_from:
                raise DuplicateFromError("Duplicate FROM")
            self.call_from = True
            if len(args) == 2:
                left, right = args
                self.data = [[l, r] for l in left for r in right]
            elif len(args) == 1:
                self.data = args[0]
            elif len(args) == 0:
                self.data=[]
            else:
                raise ValueError(f"F: {len(args)}")
            return self
        def where(self, *funcs):
            if self.where_fun:
                self.where_fun2 = funcs
            else:
                self.where_fun = funcs
            return self
        def group_by(self, *funcs):
            if self.call_group_by:
                raise DuplicateGroupByError("Duplicate GROUPBY")
            self.call_group_by = True
            self.group_fun = funcs
            return self

        def order_by(self, func=None):
            if self.call_order_by:
                raise DuplicateOrderByError("Duplicate ORDERBY")
            self.call_order_by = True
            self.order_fun = func
            return self
        def having(self, *funcs):
            self.having_fun = funcs
            return self
        def execute(self):
            if self.call_execute:
                raise Exception("Duplicate EXECUTE")
            self.call_execute = True
            if not self.call_from:
                return []
            res = self.data[:]
            if self.where_fun:
                res = [row for row in res if any(f(row) for f in self.where_fun)]
            if self.where_fun2:
                res = [row for row in res if any(f(row) for f in self.where_fun2)]
            if self.call_group_by:
                grouped = {}
                for row in res:
                    current = grouped
                    for i, func in enumerate(self.group_fun):
                        key = func(row)
                        if key not in current:
                            current[key] = [] if i == len(self.group_fun) - 1 else {}
                        current = current[key]
                    current.append(row)

                res = self._to_arr(grouped)
                if self.having_fun:
                    res = [group for group in res if all(f(group) for f in self.having_fun)]
            if self.select_fun:
                res = [self.select_fun(row) for row in res]
            if self.order_fun:
                res.sort(key=functools.cmp_to_key(self.order_fun))
            return res
        def _to_arr(self, grouped_dict):
            result = []
            for k, v in grouped_dict.items():
                key = int(k) if self._is_numeric(k) else k
                if isinstance(v, list):
                    result.append([key, v])
                else:
                    result.append([key, self._to_arr(v)])
            return result
        def _is_numeric(self, val):
            try:
                float(val)
                return True
            except (ValueError, TypeError):
                return False
    return Query()
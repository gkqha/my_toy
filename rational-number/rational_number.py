def gcd(x, y):
    ##求最大公约数
    if x == y:
        return x
    elif x < y:
        return gcd(x, y - x)
    else:
        return gcd(y, x)


def reduce(r):
    ##约分
    if isinstance(r, tuple):
        x, y = r
        if x == 0:
            return 0
        else:
            d = gcd(abs(x), y)
            if d == y:
                return x // d
            else:
                return ((x // d, y // d))
    else:
        return r


def make_frac(x, y):
    ##确保没有负数与分母不能为0
    if y == 0:
        raise Exception('0不能为分母')
    elif y < 0:
        return reduce((-x, -y))
    else:
        return reduce((x, y))


def add(r1, r2):
    if isinstance(r1, int) and isinstance(r2, int):
        return r1 + r2
    elif isinstance(r1, int) and isinstance(r2, tuple):
        return (r2[0] + r2[1] * r1, r2[1])
    elif isinstance(r1, tuple) and isinstance(r2, int):
        return (r1[0] + r1[1] * r2, r1[1])
    else:
        return reduce((r1[0] * r2[1] + r1[1] * r2[0], r1[1] * r2[1]))


def to_string(r):
    if isinstance(r, tuple):
        return str(r[0]) + "/" + str(r[1])
    else:
        return str(r)


def test_add(x, y):
    print(to_string(reduce(add(x, y))))


test_add((3, 9), (6, 9))

def fib(n):
    "Constant time Fib using Binet's formula"
    q = (1 + math.sqrt(5)) / 2
    return int(((q ** n) - (1 - q) ** n) / math.sqrt(5))

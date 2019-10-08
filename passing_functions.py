def foo(number):
	return 2 * number


def bar(f):
	return f(5)

def triple_it(value):
	return value * 3


print bar(triple_it)
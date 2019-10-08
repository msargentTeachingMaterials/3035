def foo(*args, **kwargs):
	print(args)
	print(kwargs)

a = 10

def foo2(b):   #b=a
	pass


foo(1,2,3,a=5, b=6, c=7)
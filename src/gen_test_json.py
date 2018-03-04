x = """{
  "foo" : "bar",
  "foo1" : "bar1",
  "float" : 1.1,
  "int" : 10,
  "int_is_int" : 72057594037927935,
  "int_is_float" : 1111111111
}"""

print "["
for i in xrange(0,9999):
    print x + ","
print x
print "]"

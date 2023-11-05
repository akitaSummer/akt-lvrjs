function test_1() {
  var a = { b: 1, c: { d: "1" } };

  var Person = function (name, age) {
    this.name = name;
    this.age = age;
  };

  var obj = new Person("tom", 20);

  print(obj.age);
  print(obj.name);

  print(a.b);
  print(a.c.d);
}

function test_2() {
  var b = 10;

  function f(a) {
    var ret = 0;
    for (var i = 1; ; i++) {
      if (i > a) break;
      ret += i;
    }
    return ret;
  }

  print(f(b));
}

test_1();
test_2();

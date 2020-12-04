
# coco

## A simple tool to make C++ code tidy, based on clang.

### Build

Install dependencies:

```sh
$ sudo apt install clang-10-dev cmake
```

Compiling:

```sh
$ mkdir build && cd build
$ cmake .. -DCMAKE_BUILD_TYPE=Release
$ make
```

Then you can get `coco` executable under current directory.

### Sample

Before:

```cpp
#include <iostream>

int foo() {
	return 114514;
}
int main(){
	int a,     b;
	std::cin>>a >>  b;
	std::cout<<  (a
		+b)<<std::endl;
	return 0;
}
```

After:

```cpp
#include <iostream>

int main() {
	int a, b;
	std::cin >> a >> b;
	std::cout << (a + b) << std::endl;
	return 0;
}
```

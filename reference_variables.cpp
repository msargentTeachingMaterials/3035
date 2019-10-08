#include <iostream>
using namespace std;

int x = 15;
void foo(int& i){
    i = 5;
}

int main (){
	foo(x);
	cout << x << '\n';
  	return 0;
}



#include <iostream>
using namespace std;

int main ()
{
	float stuff[100]; 
	float *p;
	p = stuff;

  for(int i = 0; i < 100; i++){
    stuff[i] = i;
  }

	cout << *(p+20) + p[30] << '\n';


  // int x = 10;
  // int* pointerToX = &x;
  // int* messedUpPointer = pointerToX - 1000000000;
  // *messedUpPointer = 300000000;


  // cout << *messedUpPointer << '\n'; 




  // int firstvalue, secondvalue;
  // firstvalue = 10;
  // cout << "location of secondvalue is " << &secondvalue << '\n';
  // // cout << "location of firstvalue is  " << &firstvalue << '\n';

  // int * mypointer;

  // mypointer = &firstvalue;
  // // cout << "value of mypointer is  " << mypointer << '\n';
  // *mypointer = 15;
  // mypointer = &secondvalue;
  // *mypointer = 20;
  // cout << "firstvalue is " << firstvalue << '\n';
  // cout << "secondvalue is " << secondvalue << '\n';

  return 0;
}
#include <stdio.h>
#include <stdlib.h>     /* srand, rand */
#include <time.h>       /* time */

void my_int_func(int x)
{
    printf( "%d\n", x );
}

void my_int_func_verbose(int x)
{
    printf( "This is the verbose version %d\n", x );
}

int main()
{
	srand(time(NULL));
	int rand_num = rand() % 10 + 1;

    void (*foo)(int);

    if(rand_num > 5){
    	foo = &my_int_func;
    }else{
    	foo = &my_int_func_verbose;
    }

    printf("The random number was: %d\n", rand_num);

    /* call my_int_func (note that you do not need to write (*foo)(2) ) */
    foo( 2 );
    /* but if you want to, you may */
    (*foo)( 2 );

    return 0;
}
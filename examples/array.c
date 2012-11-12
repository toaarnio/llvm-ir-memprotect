#include <stdio.h>

struct Foo
{
	unsigned char a;
	unsigned char b;
};


int main(void)
{
	//int* array[ 2 ];
	int a[ 255 ];
	int b[ 255 ];

	int i = 99;

	a[ i ] = 5;

	//array[ 0 ] = a;
	//array[ 1 ] = b;

	/*
	int INT_value;

	int *ptr_A;
	int *ptr_B = &INT_value;
	ptr_A = ptr_B - 10;
	*/

	/*
	int i = 99;
	array[ i ] = 44;

	printf("array %i\n", array[ i ]);
	*/

	return 0;
}


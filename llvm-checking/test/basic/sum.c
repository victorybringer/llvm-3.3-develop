#include <stdio.h>

main()
{
    int n, i, sum, prod;

    printf("Enter a positive number: ");
    scanf("%d", &n);

    sum = 0;
    prod = 1;
    i = 1;
    while (i <= n)
    {
	    sum += i;
	    prod *= i;
	    i++;
    }

    printf("n = %d, sum = %d, prod = %d\n", n, sum, prod);



#include<stdlib.h>

int foo(char *);

int main()
{
    const int MAX = 100;
    char *t = malloc(MAX);
    char *ptr = t;
    if(t == NULL)
        return 0;

    for(int i= 0; i< MAX; i++) {
        int ret = foo(t);
        if(ret !=0)
        {
            free(t);
            return (0);
        }

        t++;
    }

    free(ptr);

    return 1;
}

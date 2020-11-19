#include <stdlib.h>
#include <stdio.h>
int main()
{FILE *fp;
    char ch,filename[10];
    printf("write the name of profile£º");
    scanf("%s",filename);
    if((fp=fopen(filename,"w"))==NULL)
    {
        printf("open failed\n");
        exit(0);
    }
    ch=getchar( );
    printf("input the string (end with #)");
    ch=getchar( );
    while(ch!='#')
    {
        fputc(ch,fp);
        putchar(ch);
        ch=getchar();
        
    }
    fclose(fp);
    putchar(10);
    return 0;
}

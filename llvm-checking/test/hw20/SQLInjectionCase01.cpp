#include <stdio.h>
#include <stdlib.h>
#include "SQLAPI.h"

int TestInsertSQLCase01(const char * sql)
{
	if (sql == NULL) {
		return -1;
	}

    SAConnection con; 
    SACommand cmd;  

     try 
     { 
         con.Connect("Sun10@sqlapi","sa","haskell");
         printf("We are connected!\n");

         cmd.setConnection(&con);
		 cmd.setCommandText(sql);
		 cmd.Execute();
         con.Commit();
         printf("Table created, row inserted!\n");
         con.Disconnect();
         printf("We are disconnected!\n"); 
     } 
     catch(SAException &x){         
         printf("%s\n", (const char*)x.ErrText());
		 return -1;
     } 
	 return  0;
}


int TestInsertSQLCase02(const char * sql)
{
	if (sql == NULL) {
		return -1;
	}

	SAConnection con;
	SACommand cmd;
	SAString ss(sql);

	try {
		con.Connect(
			"Sun10@sqlapi",
			"sa", 
			"sqlapi");
		printf("We are connected!\n");

		cmd.setConnection(&con);
		cmd.setCommandText(ss);
		cmd.Execute();
		con.Commit();
		printf("Table created, row inserted!\n");
		con.Disconnect();
		printf("We are disconnected!\n");
	}
	catch (SAException &x) {
		printf("%s\n", (const char*)x.ErrText());
		return -1;
	}
	return  0;
}

int main(int argc, char* argv[])
{
	if (argc < 2) {
		return -1;
	}
	(void)TestInsertSQLCase01(argv[0]);
	(void)TestInsertSQLCase02(argv[0]);
	return 0;
}

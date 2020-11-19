enum color { red, green, yellow, blue, black};
struct abc { enum color c:2; };

int func10(){
	struct abc example;
	return 0; 
}
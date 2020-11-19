#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "securec.h"

struct FamilyInfo {
    char *father;
    char *mother;
    char *addr;
};

struct Student {
    char *name;
    unsigned int *age;
	unsigned int id;
    struct FamilyInfo *family;
    struct Student *forwardStu;
};

struct StudentChain {
    struct Student *currentStu;
    char *teacher;
    char *class;
};

/*
 * @scene 入参指针访问成员解引用后，使用指针别名判空
 */
// SVF不告警，暂时无法实现；coverity本地不告警
void reverseNullBadTest(struct StudentChain *stuChain)
{  
    struct Student *stu = stuChain->currentStu;
    // deref_ptr: Directly dereferencing pointer "stu"
    stuChain->currentStu = stuChain->currentStu->forwardStu;
    printf("The student's name is %s", stuChain->currentStu->name);
    /* POTENTIAL FLAW: check_after_deref: Null-checking "stu" suggest that it may be null,
                       but it has already been dereferenced on all paths leading to the check
    */
    if (stu == NULL) { // 本地不告警
        return;
    }
}

// 关于成员访问的判空解引用等等
// 暂不覆盖
void forwardNullBadTest(struct Student *stu) 
{
	struct Student *localstu = stu;  
	if (localstu->name == NULL) {  
		printf("the localstu's name is null"); 
	}
	memcpy_s(localstu->name, 10, "Tam", 10);
}


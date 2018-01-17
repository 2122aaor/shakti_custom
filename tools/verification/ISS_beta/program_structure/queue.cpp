#include <stdio.h>
#include <stdlib.h>
#include <string.h>
struct node{
	long BC;
	char line[70];
	struct node * next;
};

struct queue{
	struct node* head;
	struct node* tail;
};


void createqueue(struct queue * data){
	data ->head =NULL;
	data ->tail = NULL;
}

void addq(char * lineinput ,long PC,struct queue * data){
	struct node * temp = (struct node *)malloc(sizeof(struct node));
	strcpy(temp->line,lineinput);
	temp->BC = PC;
	temp->next=NULL;
	if(data->head==NULL && data->tail ==NULL)
	{
		data->head = temp;
		data->tail = temp;
	}
	else
	{
		data->tail->next=temp;
		data->tail=temp;
	}
}

void delq( struct queue * data){
	struct node * temp ;
	if(data->head != NULL){
		temp = data->head;
		data->head = data->head->next;
		free(temp);
	}
}

struct node * getline(long z ,struct queue * data){
	struct node *temp = data ->head ;
	long i=1;
	//printf("HI hello %ld\n",z);
	while(i<z){
		temp = temp->next;
		i++;
	}
	return temp;
}
void destroyqueue(struct queue * data){
	while(data->head!=NULL)
	{
		delq(data);
	}
	free(data);
}
/*
int main(){
	struct queue * data= (struct queue *)malloc(sizeof(struct queue));
	createqueue(data);
	char a[50] = "vishnu";
	char b[50] = "bhargav";
	char c[50] = "Vinod";
	char d[50] = "Krishnan";
	addq(a,1,data);
	addq(b,2,data);
	addq(c,3,data);
	addq(d,4,data);
	struct node * temp = (struct node *)malloc(sizeof(node));
	temp =getline(1,data);
	printf("Line: %s \n",temp->line);
return 0;
} */

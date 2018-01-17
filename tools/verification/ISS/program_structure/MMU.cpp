#include <stdio.h>
#include <stdint.h>
struct memnode
{
	uint64_t memloc;
	bool memval[64];
	struct memnode * next;
};

struct memqueue
{
	struct memnode* head;
	struct memnode* tail;
};

void creatememq(struct memqueue * memex)
{
	memex ->head =NULL;
	memex ->tail = NULL;
}

void addmemq(uint64_t mloc, struct memqueue * memex)
{
	struct memnode * temp = (struct memnode *)malloc(sizeof(struct memnode));
	temp->memloc = mloc;
	for(int i =0; i<64; i++)
		(temp->memval)[i] = 0;
	temp->next=NULL;
	if(memex->head==NULL && memex->tail ==NULL)
	{
		memex->head = temp;
		memex->tail = temp;
	}
	else
	{
		memex->tail->next=temp;
		memex->tail=temp;
	}
}

void memex_close(struct memqueue *memex)
{
	struct memnode * temp = (struct memnode *)malloc(sizeof(struct memnode));
	while(memex->head != NULL)
	{
		temp = memex->head;
		memex->head = memex->head->next;
		free(temp);
	}
}
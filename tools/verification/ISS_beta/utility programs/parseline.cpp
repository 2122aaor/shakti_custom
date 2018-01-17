#include <stdio.h>

void parseline(char ** parsline,char *line)
{
	int i = 0;
	size_t ln = strlen(line) - 1;
	if (line[ln-1] == 13)	//removes the extra charactes that come along with each line when using fgets
	line[ln-1] = '\0';
	char *p ;
					
	p= strtok(line," \n");	//Parses each assembly instruction to opcode, destination registers, source registers, immediates etc.
	if(p){parsline[i] = p;}
	while (parsline[i] != NULL)
	{
		i++;
		parsline[i] = strtok (NULL, " \n");
		//printf("%d ",i);
	}
}
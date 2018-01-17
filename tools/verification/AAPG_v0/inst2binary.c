#include <stdio.h>
#include <stdlib.h>

FILE * input;
FILE * output;

#include "Rreg.c"
#include "execute.c"

int main(int argc, char** argv){

	input = fopen(argv[1] ,"r");
	if(input == NULL){
		printf("Please give a valid file");
	}
	output = fopen("./AAPG/bin.s","w");

	char line[70];
	char *parsline[5];
	int inst =1;
	while(fgets(line, sizeof(line), input) != NULL)	//Reads the assembly instructions one at a time
	{
			
			inst++;
			int i = 0;
			int j;
			size_t ln = strlen(line) - 1;
			if (line[ln-1] == 13)	//removes the extra charactes that come along with each line when using fgets
	    		line[ln-1] = '\0';
	  		char *p ;
			
			p= strtok(line," \n");	//Parses each assembly instruction to opcode, destination registers, source registers, immediates etc.
			if(p){parsline[i] = p;}
			while (parsline[i] != NULL)
	  		{
	  			i++;
	    		parsline[i] = strtok (NULL, "  \n");
	    	}
	    	execute(parsline);  	//Calls the function execute() in program execute.c which in-turn calls other functions based on the opcode
	}
	fclose(output);
	fclose(input);
}

#include <stdio.h>

int main(int argc, char* argv[]) {

	unsigned int entries[2048];
	int size = 0;
	int element = 0;

	FILE* file = fopen(argv[1], "r");

	while (fscanf(file, "%d", &element) != EOF) {
		entries[size++] = element;
	}

	int inc = 0;

	for (int i = 1; i < size; i++) {
		if (entries[i-1] < entries[i]) {
			inc++;
		}
	}

	printf("pt1 count is: %d\n", inc);

	inc = 0;
	
	for (int i = 1; i < size - 2; i++) {
		int first = entries[i-1] + entries[i] + entries[i+1];
		int second = entries[i] + entries[i+1] + entries[i+2];
		if (second > first) {
			inc++;
		}
	}

	printf("pt2 count is: %d\n", inc);

	fclose(file);

	return 0;
}

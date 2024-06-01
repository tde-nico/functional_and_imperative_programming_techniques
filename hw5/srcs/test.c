#include "HW5-TommasoDeNicola.c"
#include <time.h>

void	print_tree(t_cbintree *tree)
{
	if (!tree)
		return ;
	printf("%d,%d,%d\n", tree->n, tree->k, tree->value);
	print_tree(tree->left);
	print_tree(tree->right);
}

void print_euler_sieve(t_pair *p, int n)
{
	int	pos;

	if (!p)
		return ;
	pos = 0;
	while (pos >= 0 && pos < n)
	{
		printf("%d %d %d\n", pos, p[pos].prec, p[pos].succ);
		pos += p[pos].succ;
	}	
}

void	print_eratosthenes_sieve(int * p)
{
	while (*p)
		printf("%d\n", *p++);
}

int	main(void)
{
	endianess();
	// Little-endian architecture

	printf("----------\n");

	int count;
	int i;
	int	arr[] = {1, 9, 2, 6, 3, 4, 5, 10, 6, 7, 9, 4, 8, 1, 10, 11};
	count = removeDups(arr, sizeof(arr) / sizeof(int));
	printf("count: %d\narr = [", count);
	i = -1;
	while (++i < count)
		printf("%d,", arr[i]);
	printf("]\n");
	// [1,9,2,6,3,4,5,10,7,8,11,]
	int	arr2[] = {5, 2, 1, 2, 5, 7, 2, 1, 2, 7};
	count = removeDups(arr2, sizeof(arr2) / sizeof(int));
	printf("count: %d\narr = [", count);
	i = -1;
	while (++i < count)
		printf("%d,", arr2[i]);
	printf("]\n");
	// [5,2,1,7,]

	printf("----------\n");

	t_cbintree	*tree = malloc(sizeof(t_cbintree));
	cBinInvocationSharing(5, 3, &tree, tree);
	print_tree(tree);
	/*
	5,3,10
	4,2,6
	3,1,3
	2,0,1
	2,1,2
	1,0,1
	1,1,1
	3,2,3
	2,1,2
	1,0,1
	1,1,1
	2,2,1
	4,3,4
	3,2,3
	2,1,2
	1,0,1
	1,1,1
	2,2,1
	3,3,1
	*/

	printf("----------\n");

	int n = 10000000; // 10_000_000
	clock_t	start = clock();
	t_pair	*p = eulerSieve(n);
	clock_t	end = clock();
	printf("Time taken: %ld\n", end - start);
	(void)p;
	//print_euler_sieve(p, n);

	clock_t	start2 = clock();
	int	*p2 = eratosthenesSieve(n);
	clock_t	end2 = clock();
	printf("Time taken: %ld\n", end2 - start2);
	(void)p2;
	//print_eratosthenes_sieve(p2);

	/*
	Time taken: 451381
	Time taken: 70783
	*/

	return (0);
}

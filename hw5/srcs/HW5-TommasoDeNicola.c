#include <stdio.h>
#include <string.h>
#include <stdlib.h>


// ES 1: Type Safety

typedef union u_endianness {
	unsigned int num;
	unsigned char bytes[4];
}	t_endianess;

void	endianess(void)
{
	t_endianess	test;
	
	test.num = 0x12345678;
	if (test.bytes[0] == 0x12)
		printf("Big-endian architecture\n");
	else
		printf("Little-endian architecture\n");
}


// ES 2: Array Mutabili

typedef struct s_element {
	int	value;
	int	index;
}	t_element;

int	cmp_first(const void *a, const void *b)
{
	int	ret;

	ret = (*(t_element *)a).value - (*(t_element *)b).value;
	if (ret)
		return (ret);
	return ((*(t_element *)a).index - (*(t_element *)b).index);
}

int	cmp_second(const void *a, const void *b)
{
	return ((*(t_element *)a).index - (*(t_element *)b).index);
}

int	removeDups(int *arr, int n)
{
	int			count;
	int			i;
	t_element	*tmp1;
	t_element	*tmp2;

	tmp1 = malloc(sizeof(t_element) * n);
	if (!tmp1)
		return (-1);
	tmp2 = malloc(sizeof(t_element) * n);
	if (!tmp2)
		return (-1);

	i = -1;
	while (++i < n)
	{
		tmp1[i].value = arr[i];
		tmp1[i].index = i;
	}

	qsort(tmp1, n, sizeof(t_element), cmp_first);

	i = -1;
	count = 0;
	while (++i < (n - 1))
	{
		tmp2[count].value = tmp1[i].value;
		tmp2[count].index = tmp1[i].index;
		count++;
		while (tmp1[i].value == tmp1[i + 1].value && i < (n - 2))
			++i;
	}
	if (tmp1[i - 1].value != tmp1[i].value)
	{
		tmp2[count].value = tmp1[i].value;
		tmp2[count].index = tmp1[i].index;
		count++;
	}

	qsort(tmp2, count, sizeof(t_element), cmp_second);

	i = -1;
	while (++i < count)
		arr[i] = tmp2[i].value;

	return (count);
}


// ES 3: Quello che in Haskell non si puo fare I: Dati "non-funzionali"

typedef struct s_cbintree {
	int					n;
	int					k;
	int					value;
	struct s_cbintree	*left;
	struct s_cbintree	*right;
}	t_cbintree;

int	cbin(int n, int k)
{
	if (n == k || n == 0)
		return 1;
	return (cbin(n-1,k-1) + cbin(n-1,k));
}

t_cbintree	*search_tree(t_cbintree *tree, int n, int k)
{
	if (tree == NULL)
		return NULL;
	if (tree->n == n && tree->k == k)
		return tree;
	t_cbintree	*left = search_tree(tree->left, n, k);
	if (left != NULL)
		return left;
	return search_tree(tree->right, n, k);
}

int	cBinInvocationSharing(int n, int k, t_cbintree **t, t_cbintree *r)
{
	t_cbintree	*found = search_tree(r, n, k);
	if (found != NULL)
	{
		*t = found;
		return (*t)->value;
	}
	if (n == k || n <= 0 || k <= 0)
	{
		(*t)->n = n;
		(*t)->k = k;
		(*t)->value = 1;
		(*t)->left = NULL;
		(*t)->right = NULL;
		return 1;
	}
	(*t)->n = n;
	(*t)->k = k;
	(*t)->left = malloc(sizeof(t_cbintree));
	if ((*t)->left == NULL)
		return -1;
	(*t)->right = malloc(sizeof(t_cbintree));
	if ((*t)->right == NULL)
	{
		free((*t)->left);
		return -1;
	}
	(*t)->value = cBinInvocationSharing(n-1, k-1, &(*t)->left, r)
		+ cBinInvocationSharing(n-1, k, &(*t)->right, r);
	return (*t)->value;
}


// ES 4: Quello che in Haskell non si puo fare II: Crivello di Eulero

typedef struct s_pair {
	int	succ;
	int	prec;
}	t_pair;

void	intset(void *s, int c, size_t n)
{
	int	*ptr;

	ptr = (int *)s;
	while (n--)
		*ptr++ = c;
}

t_pair	*eulerSieve(int n)
{
	t_pair	*p;
	int		pos;
	int		i;

	p = malloc(sizeof(t_pair) * (n + 1));
	if (!p)
		return (NULL);
	intset(p, 1, (n + 1) * 2);

	pos = 2;
	while ((pos * pos) < n)
	{
		i = pos * pos;
		while (i <= n)
		{		
			if (i + p[i].succ < n)
				p[i + p[i].succ].prec += p[i].prec;
			p[i - p[i].prec].succ += p[i].succ;
			p[i].prec = 0;
			p[i].succ = 0;
			i += pos;
		}
		pos += p[pos].succ;
	}

	return (p);
}

int	*eratosthenesSieve(int n)
{
	u_int8_t	*is_composite;
	int			*p;
	int			last_prime;
	int			i;
	int			j;

	is_composite = malloc(sizeof(u_int8_t) * (n + 1));
	if (!is_composite)
		return (NULL);
	memset(is_composite, 0, n + 1);
	p = malloc(sizeof(int) * n);
	if (!p)
		return (NULL);
	memset(p, 0, n);
	i = 1;
	last_prime = 0;
	while (++i < n)
	{
		if (!is_composite[i])
			p[last_prime++] = i;
		j = 0;
		while (j < last_prime && i * p[j] < n)
		{
			is_composite[i * p[j]] = 1;
			if (!(i % p[j++]))
				break ;
		}
	}

	return (p);
}

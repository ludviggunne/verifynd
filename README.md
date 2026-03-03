# verifynd

This is a tool for verifying proofs in [natural deduction](https://plato.stanford.edu/entries/natural-deduction/).

```
// Proof of (p || q) -> r |- (p -> r) && (q -> r)

1	(p || q) -> r			premise;
	{
2		p					assumption;
3		p || q				||i1 2;
4		r					->e 1, 3;
	}
5	p -> r					->i 2-4;
	{
6		q					assumption;
7		p || q				||i2 6;
8		r					->e 1, 7;
	}
9	q -> r					->i 6-8;
10	(p -> r) && (q -> r)	&&i 5, 9;
```

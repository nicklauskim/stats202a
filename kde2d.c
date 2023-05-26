#include <R.h> 
#include <Rmath.h>

void kde2(int *m, int *n, double *g2, double *b, double *x, double *y, double *res2) 
/* x will be the data vector of length n,
b is the bandwidth,
g will be the vector of m gridpoints where we want to calculate the kernel regression estimates
*/
{
    for (int i = 0; i < *m; i++) 
    {
        double numer = 0;
        double denom = 0; 
        for (int j = 0; j < *n; j++) 
        {
            numer += (dnorm(((double) g2[i] - (double) x[j]), 0, *b, 0) * y[j]);
            denom += dnorm(((double) g2[i] - (double) x[j]), 0, *b, 0);
        }
        res2[i] = numer / denom;
    }
}
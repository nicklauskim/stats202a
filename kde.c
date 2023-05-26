#include <R.h>
#include <Rmath.h>


void kde(int *m, int *n, double *g, double *b, double *x, double *y)
/* x will be the data vector of length n, 
b is the bandwidth,
g will be the vector of m gridpoints where we want to calculate the 
kernel regression estimates
*/
{
    for (int i = 0; i < *m; i++)
    {
        double dist_from_ith = 0;
        for (int j = 0; j < *n; j++)
        {
            dist_from_ith += dnorm(((double) x[j] - (double) g[i]), 0, *b, 0);
        }
        y[i] = (double) dist_from_ith / (double) *n;
    }
}


/*
void kernreg2 (double *x, double *y, int *n, double *b,
	       double *g, int *m, double *est)
{
    int i,j;
    double a1,a2,c;
    for(i = 0; i < *m; i++){
	a1 = 0.0;
	a2 = 0.0;
	for(j=0; j < *n; j++){
	    if(fabs((x[j] - g[i])/ *b) <= 1.0){
		c = 0.75 * (1.0-pow((x[j]-g[i])/ *b,2.0))/ *b;
		a1 += y[j] * c;	
		a2 += c;
	    }
	}
	if(a2 > 0.0) est[i] = a1/a2; 
	else est[i] = 0.0;
	Rprintf("I'm on gridpoint %f and index %d right now.\n", g[i], i);
    }
}
*/
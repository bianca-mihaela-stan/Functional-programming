#include <iostream>
#include <fstream>
#include <bits/stdc++.h>
const int nmax=50000;
const int inf =1e9;
using namespace std;
ifstream f ("grafpond.in");
int v[nmax][nmax];
int diametru=0;
int radius=inf;
int eccentricity[nmax];
int center;
int perifery[nmax];
int main()
{
    int n, m;
    f>>n>>m;
    for(int i=0; i<m; i++)
    {
        int x, y,z;
        f>>x>>y>>z;
        v[x][y]=z;
    }
    for(int i=1; i<=n; i++)
    {
        for(int j=i+1; j<=n; j++)
        {
            for(int k=1; k<=n; k++)
            {
                if (v[i][j]> v[i][k]+v[k][j])
                    v[i][j]=v[i][k]+v[k][j];
                eccentricity[i]=max(eccentricity[i], v[i],[j]);
            }
        }
        diametru=max(diametru, eccentricity[i]);
        radius=min(radius, eccentricity[i]);
    }
    for(int i=1; i<=n; i++)
    {
        if (radius==eccentricity[i]) center=i;
    }
    int p=0;
    for(int i=1; i<=n; i++)
    {
        if(diametru==eccentricity[i]) {perifery[p]=i; p++;}
    }
    return 0;
}

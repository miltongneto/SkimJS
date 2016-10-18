#include <bits/stdc++.h>

using namespace std;


vector <int> tam (N);
vector <int> val (N);

int pd (int S, N)
{
    int t[N][S];

    for (int i = 0; i < S; ++i)
    {
        for (int j = 0; j < N; ++j)
        {
            if (i == 0 || j == 0) t[i][j];
            else if (tam[i-1] <= j)
            {
                t[i][j] = max(t[i-1][j - tam[i-1]] + val, k[i-1][j]);
            }

        }
    }


}


int main(int argc, char const *argv[])
{
    int S, N;
    cin >> S >> N;  




    return 0;
}
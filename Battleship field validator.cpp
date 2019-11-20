#include <vector>
using namespace std;

bool validate_battlefield(vector<vector<int>>f){
int r=0;
  for(int i=0;i<10;i++)for(int j=0;j<10;j++)if(f[i][j])
  for(int a=max(i-1,0);a<min(i+2,10);a++)
  for(int b=max(j-1,0);b<min(j+2,10);b++)r+=f[a][b];
return r==40;
}
#include <Rcpp.h>
#include <vector>
#include <stack>
# define PI 3.14159265358979323846
using namespace Rcpp;

//' @title Implementation of the EDG subroutine
//' @description Returns which edge of I is adjacent to J
//' @param I index of triangles I
//' @param J index of triangle J
//' @param e List of adjacent triangles
// [[Rcpp::export]]
int Edge(int I,
         int J,
         std::vector<std::vector<int>> e){
  for(int i = 1; i<=3;i++){
    if(e[i][I] == J){
      return i;
    }
  }
  return -1;
}

//' @title Cross Product of AB and AP
//' @description Calculates cross product of AP and AP to check if P is left or right of AB
//' @param Px X component of point P
//' @param Py Y component of point P
//' @param Ax X component of point A
//' @param Ay Y component of point A
//' @param Bx X component of point B
//' @param By Y component of point B
//' @importFrom Rcpp evalCpp
//' @exportPattern ^[[:alpha:]]+
//' @useDynLib triad, .registration=TRUE
//' @export
// [[Rcpp::export]]
double LeftRight(double Px, double Py, double Ax, double Ay, double Bx, double By){
  double ABx = Bx-Ax;
  double ABy = By-Ay;
  double APx = Px-Ax;
  double APy = Py-Ay;
  double cross_prod = ABx*APy - APx*ABy;
  return cross_prod;
}


//'@title Implementation of the SWAP subroutine
//'@description Checks to see if triangle P-V2-V1 and V3-V1-V2 need to swap common edge
//'@param x1 X coordinate of V1
//'@param y1 Y coordinate of V1
//'@param x2 X coordinate of V2
//'@param y2 Y coordinate of V2
//'@param x3 X coordinate of V3
//'@param y3 Y coordinate of V3
//'@param xp X coordinate of P
//'@param yp Y coordinate of P
//'@export
//[[Rcpp::export]]
bool Swap(double x1,
          double y1,
          double x2,
          double y2,
          double x3,
          double y3,
          double xp,
          double yp){
  // angle between vector p->1 and p->2
  double x1p = x1 - xp;
  double y1p = y1 - yp;
  double x2p = x2 - xp;
  double y2p = y2 - yp;
  double dot_1p_2p = x1p*x2p + y1p*y2p;
  double mod_1p = std::sqrt(x1p*x1p + y1p*y1p);
  double mod_2p = std::sqrt(x2p*x2p + y2p*y2p);
  double cosalpha = dot_1p_2p/(mod_1p*mod_2p);
  if(cosalpha>1){
    cosalpha = 1;
  }
  if(cosalpha<-1){
    cosalpha = -1;
  }
  double alpha = std::acos(cosalpha);
  
  //angle between vector 3->1 and 3->2
  double x13 = x1 - x3;
  double y13 = y1 - y3;
  double x23 = x2 - x3;
  double y23 = y2 - y3;
  double dot_13_23 = x13*x23 + y13*y23;
  double mod_13 = std::sqrt(x13*x13+y13*y13);
  double mod_23 = std::sqrt(x23*x23+y23*y23);
  double cosgamma = dot_13_23/(mod_13*mod_23);
  if(cosgamma > 1){
    cosgamma = 1;
  }
  if(cosgamma < -1){
    cosgamma = -1;
  }
  double gamma = std::acos(cosgamma);
  return ((alpha+gamma)>PI);
}




//'@title Check which triangles a point lies within
//'@description Returns the index of the triangle which contains the given point
//'@param i index of the point to be located
//'@param x X coordinates of points
//'@param y Y coordinates of points
//'@param v Vertices of triangles, columns are triangle number and rows are vertices 1,2,3
//'@param e Adjacency of triangles, columns are triangle number and rows are ids of adjacent triangles
//'@export
//[[Rcpp::export]]
int TriLoc(int i,
           std::vector<double> x,
           std::vector<double> y,
           std::vector<std::vector<int>> v,
           std::vector<std::vector<int>> e){
  int cur_tri = 1;
  //arc1 is opposite v1
  bool tri_found = false;
  double Px = x[i];
  double Py = y[i];
  while(!tri_found){
    //edge1->2
    double Ax = x[v[1][cur_tri]];
    double Ay = y[v[1][cur_tri]];
    double Bx = x[v[2][cur_tri]];
    double By = y[v[2][cur_tri]];
    double lr12 = LeftRight(Px,Py,Ax,Ay,Bx,By);
    if(lr12<0){
      cur_tri = e[1][cur_tri];
      continue;
    }
    //edge2->3
    Ax = x[v[2][cur_tri]];
    Ay = y[v[2][cur_tri]];
    Bx = x[v[3][cur_tri]];
    By = y[v[3][cur_tri]];
    double lr23 = LeftRight(Px,Py,Ax,Ay,Bx,By);
    if(lr23<0){
      cur_tri = e[2][cur_tri];
      continue;
    }
    //edge 3->1
    Ax = x[v[3][cur_tri]];
    Ay = y[v[3][cur_tri]];
    Bx = x[v[1][cur_tri]];
    By = y[v[1][cur_tri]];
    double lr31 = LeftRight(Px,Py,Ax,Ay,Bx,By);
    if(lr31<0){
      cur_tri = e[3][cur_tri];
      continue;
    }
    return cur_tri;
  }
  return -1;
}

//'@export
//[[Rcpp::export]]
int test_TriLoc(int i,
                NumericVector x,
                NumericVector y,
                NumericMatrix v,
                NumericMatrix e){
  int n = x.size();
  std::vector<double> xv(n+1);
  std::vector<double> yv(n+1);
  int ncol = v.ncol();
  std::vector<int> aux(ncol+1);
  std::vector<std::vector<int>> vv(4,aux);
  std::vector<std::vector<int>> ev(4,aux);
  for(int i = 1;i<=n;i++){
    xv[i] = x[i-1];
    yv[i] = y[i-1];
  }
  
  for(int i = 1; i<=3;i++){
    for(int j =1; j <= ncol; j++){
      vv[i][j] = v(i-1,j-1);
      ev[i][j] = e(i-1,j-1);
    }
  }
  
  return TriLoc(i,xv,yv,vv,ev);
  
}

//'@title Implementation of the DELAUN subroutine
//'@description Returns a Delaunay Triangulation but with normalised points
//'@param norm_x Normalised X coordinates of points
//'@param norm_y Normalised Y coordinates of points
//'@export
//[[Rcpp::export]]
NumericMatrix Delaun(NumericVector norm_x, 
                                     NumericVector norm_y){
  int numpts = norm_x.size();
  std::vector<double> x(numpts+3+1,0); // +3 for supertriangle, +1 to use 1-indexing in c++
  std::vector<double> y(x);
  for(int i = 0; i < numpts; i++){
    x[i+1] = norm_x[i];
    y[i+1] = norm_y[i];
  }
  
  //define vertex and adjacency list
  std::vector<int> aux_vector{0,0};
  std::vector<std::vector<int>> v(3+1,aux_vector);
  int v1 = numpts+1;
  int v2 = numpts+2;
  int v3 = numpts+3;
  v[1][1] = v1;
  v[2][1] = v2;
  v[3][1] = v3;
  
  std::vector<std::vector<int>> e(3+1,aux_vector);
  
  //reserve memory for speed purposes
  v[1].reserve(2*numpts+1+3);
  v[2].reserve(2*numpts+1+3);
  v[3].reserve(2*numpts+1+3);
  e[1].reserve(2*numpts+1+3);
  e[2].reserve(2*numpts+1+3);
  e[3].reserve(2*numpts+1+3);
  
  //set coords of supertriangle
  x[v1] = -100;
  x[v2] = 100;
  x[v3] = 0;
  
  y[v1] = -100;
  y[v2] = -100;
  y[v3] = 100;
  
  //loop over each point
  int numtri = 1;
  std::stack<int> tstack;
  for(int i = 1; i <= numpts; i++){
    double xp = x[i];
    double yp = y[i];
    //locate triangle in which point lies
    int t = TriLoc(i,x,y,v,e);
    
    //create new vertex and adjacency list for triangle t
    int a = e[1][t];
    int b = e[2][t];
    int c = e[3][t];
    
    v1 = v[1][t];
    v2 = v[2][t];
    v3 = v[3][t];
    
    v[1][t] = i;
    v[2][t] = v1;
    v[3][t] = v2;
    e[1][t] = numtri+2;
    e[2][t] = a;
    e[3][t] = numtri+1;
    // create new triangles
    for(int j = 1; j <=3;j++){
      v[j].push_back(0);
      v[j].push_back(0);
      e[j].push_back(0);
      e[j].push_back(0);
    }
    numtri++;
    v[1][numtri] = i;
    v[2][numtri] = v2;
    v[3][numtri] = v3;
    e[1][numtri] = t;
    e[2][numtri] = b;
    e[3][numtri] = numtri+1;
    
    numtri++;
    v[1][numtri] = i;
    v[2][numtri] = v3;
    v[3][numtri] = v1;
    e[1][numtri] = numtri-1;
    e[2][numtri] = c;
    e[3][numtri] = t;
    // put each edge of triangle t on stack
    if(a != 0){
      tstack.push(t);
    }
    if(b != 0){
      e[Edge(b,t,e)][b] = numtri-1;
      tstack.push(numtri-1);
    }
    if(c!=0){
      e[Edge(c,t,e)][c] = numtri;
      tstack.push(numtri);
    }
    // loop while stack is not empty
    while(!tstack.empty()){
      int l = tstack.top();
      tstack.pop();
      int r = e[2][l];
      //check if new point is in circumcircle for triangle r
      int erl = Edge(r,l,e);
      int era = erl%3 + 1;
      int erb = erl%3 + 1;
      v1 = v[erl][r];
      v2 = v[era][r];
      v3 = v[erb][r];
      bool swap_true = Swap(x[v1],y[v1],x[v2],y[v2],x[v3],y[v3],xp,yp);
      if(swap_true){
        // new  point is inside circumcircle for triangle r
        //swap diagonal for convex quad formed by P V2 V3 V1
        a = e[era][r];
        b = e[erb][r];
        c = e[3][l];
        //update vertex and adjacency list for triangle l
        v[3][l] = v3;
        e[2][l] = a;
        e[3][l] = r;
        //update vertex and adjacency list for triangle r
        v[1][r] = i;
        v[2][r] = v3;
        v[3][r] = v1;
        e[1][r] = l;
        e[1][r] = b;
        e[1][r] = c;
        // put edges l-a and r-b on stack
        // update adjacency lists for triangles a and c
        if(a!=0){
          e[Edge(a,r,e)][a] = l;
          tstack.push(l);
        }
        if(b!=0){
          tstack.push(r);
        }
        if(c!=0){
          e[Edge(c,l,e)][c] = r;
        }
      }
    }
  }
  //check consistency of triangulation
  int t = 1;
  while(t <= numtri){
    if(v[1][t] > numpts || v[2][t] > numpts || v[3][t] > numpts){
      v[1].erase(v[1].begin() + t);
      v[2].erase(v[2].begin() + t);
      v[3].erase(v[3].begin() + t);
      numtri--;
    }else{
      t++;
    }
  }
  numtri = v[1].size();
  NumericMatrix v_new(3,numtri);
  for(int i = 1; i<=3; i++){
    for(int j = 1; j<=numtri;j++){
      v_new(i-1,j-1) = v[i][j];
    }
  }
  return v_new;
}

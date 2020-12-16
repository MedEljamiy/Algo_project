


#include <Rcpp.h>
#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include <cstdlib>
#include <cmath>
#include <sstream>
#include <ctime>


using namespace Rcpp;
// [[Rcpp::export]]
class City
{
public:
  City();
  City(double x, double y);
  
  void setX(double x);
  void setY(double y);
  double getX() const;
  double getY() const;
  
  static double dist(City &p, City &q);
  
private:
  double x;
  double y;
};

City::City() : x(0), y(0)
{}

City::City(double x, double y) : x(x), y(y)
{}

void City::setX(double x)
{
  this->x = x;
}

void City::setY(double y)
{
  this->y = y;
}

double City::getX() const
{
  return this->x;
}

double City::getY() const
{
  return this->y;
}

double City::dist(City &p, City &q)
{
  double dx = p.x - q.x;
  double dy = p.y - q.y;
  double result = sqrt(dx*dx + dy*dy);
  return result;
}





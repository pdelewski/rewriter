int foo() 
{
  return 1;
}

void bar(int x) 
{}

int main() 
{
  {
    int x = foo();
  }
  {
    int x;
    x = foo();
  }
  {
    bar(foo());
  }
  
  return 0;
}
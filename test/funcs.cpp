// clang -Xclang -ast-dump funcs.cpp

int foo() 
{
  return 1;
}

int foo2()
{
  return 2;
}

void bar(int x) 
{}

void call() 
{
  bar(foo());
}

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
    call();
  }
  
  return 0;
}

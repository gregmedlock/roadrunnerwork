#include "Poco/Thread.h"
#include "Poco/Runnable.h"
#include <iostream>

using namespace std;
using namespace rr;

class HelloRunnable: public Poco::Runnable
{
    virtual void run()
    {
        std::cout << "Hello, world!" << std::endl;
    }
};

int main(int argc, char** argv)
{
    HelloRunnable runnable;
    Poco::Thread thread;
    thread.start(runnable);
    thread.join();
    return 0;
}


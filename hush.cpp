#include <string>
#include <cstring>
#include <iostream>
#include <sstream>

using namespace std;

struct HushString
{
    void *data;
    uintptr_t size;

    HushString(string s)
    {
        size = s.length();
        data = malloc(size);
        memcpy(data, s.c_str(), size);
    }
};

extern "C" void hush_show_uint(HushString *str, uintptr_t i)
{
    stringstream s;
    s << i;
    *str = HushString(s.str());
}

extern "C" void hush_show_int(HushString *str, intptr_t i)
{
    stringstream s;
    s << i;
	*str = HushString(s.str());
}

extern "C" void hush_puts(char *c, size_t size)
{
    cout << string(c, size);
}

extern "C" void hush_gets(HushString *str)
{
    string s;
    getline(cin, s);
	*str = HushString(s);
}

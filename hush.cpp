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

extern "C" HushString hush_show_uint(uintptr_t i)
{
    stringstream s;
    s << i;
    return HushString(s.str());
}

extern "C" HushString hush_show_int(intptr_t i)
{
    stringstream s;
    s << i;
    return HushString(s.str());
}

extern "C" void hush_puts(char *c, size_t size)
{
    cout << string(c, size);
}

extern "C" HushString hush_gets()
{
    string s;
    getline(cin, s);
	return HushString(s);
}

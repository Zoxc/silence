#include <string>
#include <cstring>
#include <cstdlib>
#include <iostream>
#include <sstream>

using namespace std;

struct HushString
{
    const char *list;
    uintptr_t size;
    uintptr_t capacity;
    struct {} allocator;

    HushString(string s)
    {
        size = s.length();
        capacity = size;
        void *data = malloc(size);
        memcpy(data, s.c_str(), size);
		list = (const char *)data;
    }

    string str()
    {
		return string(list, size);
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

extern "C" void hush_puts(HushString *str)
{
    cout << str->str();
}

extern "C" void hush_gets(HushString *str)
{
    string s;
    getline(cin, s);
	*str = HushString(s);
}

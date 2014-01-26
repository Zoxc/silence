import malloc(size uint) -> *()
import free(mem *())
import memcpy(dst *(), src *(), len uint)
import puts(str *char)
import putchar(c c_int) -> c_int

struct C
	struct FILE

	shared import fopen(file *char, mode *char) -> *FILE
	shared import fgetc(file *FILE) -> c_int
	shared import fclose(file *FILE) -> ()
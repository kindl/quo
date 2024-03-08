// TODO opaque types like
// extern struct FILE;

extern int puts(UnsafePointer<char> str);
extern UnsafeMutablePointer<FILE> fopen(UnsafePointer<char> filename, UnsafePointer<char> mode);
extern int fgetc(UnsafeMutablePointer<FILE> stream);
extern int fputc(int character, UnsafeMutablePointer<FILE> stream );
extern int fclose(UnsafeMutablePointer<FILE> stream);
extern UnsafeMutablePointer<void> malloc(usize size);

int eof = -1;

/*
TODO inline
T alloc<T>() {
    return cast<T>(malloc(sizeof<T>()));
}

void arrayStuff() {
    auto arr = [];
}
*/

// This will run out of stack space, but is just for testing if recursion works
void copyRecursively(UnsafeMutablePointer<FILE> fileHandle1, UnsafeMutablePointer<FILE> fileHandle2) {
    auto c = fgetc(fileHandle1);
    if (c != eof) {
        fputc(c, fileHandle2);
        copyRecursively(fileHandle1, fileHandle2);
    }
}

void copyWithLoop(UnsafeMutablePointer<FILE> fileHandle1, UnsafeMutablePointer<FILE> fileHandle2) {
    while (true) {
        auto c = fgetc(fileHandle1);
        if (c == eof)
        {
            return;
        }

        fputc(c, fileHandle2);
        copyRecursively(fileHandle1, fileHandle2);
    }
}

int main() {
    auto fileHandle1 = fopen(fileName, "r\0");

    puts("Done\0");

    if (fileHandle1 == nullptr) {
        return -1;
    }

    auto fileHandle2 = fopen(fileName, "w\0");
    if (fileHandle2 == nullptr) {
        return -1;
    }

    copyWithLoop(fileHandle1, fileHandle2);

    fclose(fileHandle1);
    fclose(fileHandle2);

    return 0;
}

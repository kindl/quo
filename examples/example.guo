fn main() i32 {
    let fileHandle1 = fopen(fileName, "r\0")
    if (fileHandle1 == nullptr) {
        return 1
    }

    let fileHandle2 = fopen(fileName, "w\0")
    if (fileHandle2 == nullptr) {
        return 1
    }

    puts("Done\0")

    fclose(fileHandle1)
    fclose(fileHandle2)

    return 0
}

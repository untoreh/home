import std/asyncfile
proc writeFileAsync(relpath: string, content: auto) {.async.} =
    let dir = relpath.parentDir
    if not dir.dirExists:
        createDir(dir)
    var file = openAsync(relpath, fmReadWrite)
    let prom = file.write(content)
    await prom
    file.close()

proc readFileAsync(path: string, page: ptr string) {.async.} =
    var file = openAsync(path, fmRead)
    defer: file.close()
    page[] = await file.readAll()

proc readFileAsync(path: string): Future[string] {.async.} =
    var file = openAsync(path, fmRead)
    defer: file.close()
    return await file.readAll()

proc writeFileAsync(relpath: string, content: auto) {.async.} =
    let dir = relpath.parentDir
    if not dir.dirExists:
        createDir(dir)
    var file = openAsync(relpath, fmReadWrite)
    let prom = file.write(content)
    await prom
    file.close()

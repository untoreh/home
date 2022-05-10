import exitprocs

let tmpDir = create(string)
tmpDir[] = createTempDir("nim", "flw")
proc clean() =
    removeDir(tmpDir[])
addExitProc(clean)

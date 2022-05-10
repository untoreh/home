import httpcore,
       fusion/matching,
       guildenstern/[ctxheader, ctxbody]

{.experimental: "caseStmtMacros".}

template handleSearch(relpath: string, ctx: HttpCtx) =
    var headers: array[1, string]
    ctx.parseHeaders(["abc"], headers) # OFFENDING LINE

proc handleGet(ctx: HttpCtx) {.gcsafe, raises: [].} =
    doassert ctx.parseRequestLine
    var
        relpath = ctx.getUri()
        page: string
    try:
        let capts = (page: "")
        case capts:
            of (page: "s"):
                handleSearch(relpath, ctx)
            else:
                discard
    except:
        try:
            let msg = getCurrentExceptionMsg()
        except:
            ctx.reply(Http501)
            discard
        discard

when isMainModule:

    var server = new GuildenServer
    server.initHeaderCtx(handleGet, 5050, false)
    echo "GuildenStern HTTP server serving at 5050"
    server.serve(loglevel = INFO)

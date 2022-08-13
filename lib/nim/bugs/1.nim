import karax/[vdom, karaxdsl]
import macros

macro two(s: static[string]): auto =
  let nodes = buildHtml(tdiv()):
    link(href=s)
    link(href=s)
  quote do:
    echo `nodes`
    newVNode(VNodeKind.verbatim)
    # for n in `nodes`:
    #   n

proc one(): VNode =
  buildHtml(tdiv):
    tdiv(class="1")
    tdiv(class="2")
    two("abc")

echo one()

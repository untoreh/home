set -gx GOENV_SHELL fish
set -gx GOENV_ROOT /home/fra/.goenv
if not contains $GOENV_ROOT/shims $PATH
  set -gx PATH $GOENV_ROOT/shims $PATH
end
source '/home/fra/.goenv/libexec/../completions/goenv.fish'
#if type -q goenv
#	command goenv rehash 2>/dev/null
#end
function goenv
  set command $argv[1]
  set -e argv[1]

  switch "$command"
  case rehash shell
    source (goenv "sh-$command" $argv|psub)
  case '*'
    command goenv "$command" $argv
  end
end
goenv rehash --only-manage-paths

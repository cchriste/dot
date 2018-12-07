#Sunday, October 21, 2018
#utils.sh
#shared with enable_* scripts

add_to_path()
{
  case ":$PATH:" in
    *":$1:"*) :;;         # already there: ignore
    *) PATH="$1:$PATH";;  # add to PATH
  esac

  # this is what the case statement above does:
#  echo $PATH | grep -q -s "/usr/local/bin"
#  if [ $? -eq 1 ] ; then
#    PATH=/usr/local/bin:$PATH
#  fi
}


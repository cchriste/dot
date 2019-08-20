#Sunday, October 21, 2018
#utils.sh
#shared with enable_* scripts

# example: is a variable set already? If not, give it a default value
# if [ -z $FOO ]; then
#   FOO=0
# fi

add_to_path()
{
  case ":$PATH:" in
    *":$1:"*) :;;         # already there: ignore
    *) PATH="$1:$PATH";;  # add to PATH
  esac

# this is what the (obscure) case statement above does:
#  echo $PATH | grep -q -s "/usr/local/bin"
#  if [ $? -eq 1 ] ; then
#    PATH=/usr/local/bin:$PATH
#  fi
}

#
# test two versions of something (in this case, gcc)
#
# example usage:
#
# currentver="$(gcc -dumpversion)"
# requiredver="5.0.0"
# at_least_required_version $currentver $requiredver
#
at_least_required_version()
{
  current=$1
  required=$2
  if [ "$(printf '%s\n' "$required" "$current" | sort -V | head -n1)" = "$required" ]; then 
    echo "Greater than or equal to $required"
    return 1
  else
    echo "Less than $required"
    return 0
  fi
}

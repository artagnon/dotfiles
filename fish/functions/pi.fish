# Defined in - @ line 1
function pi --wraps='pip install' --description 'alias pi=pip install'
  pip install $argv;
end

# Defined in - @ line 1
function gi --wraps='gem install' --description 'alias gi=gem install'
  gem install $argv;
end

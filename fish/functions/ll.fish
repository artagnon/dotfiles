# Defined via `source`
function ll --wraps='ls -lha' --description 'alias ll=ls -lha'
  ls -lha $argv; 
end

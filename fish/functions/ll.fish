function ll --wraps='ls -lha' --wraps='exa --long' --description 'alias ll=exa --long'
  exa --long $argv; 
end

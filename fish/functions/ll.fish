function ll --wraps='ls -lha' --wraps='eza --long' --description 'alias ll=eza --long'
  eza --long $argv; 
end

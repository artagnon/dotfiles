function tb --wraps='nc termbin.com 9999' --description 'alias tb=nc termbin.com 9999'
  nc termbin.com 9999 $argv; 
end

syntax on
set nocp
set is
set hls
set hid
set ts=8
set sw=4
set bs=2
set et
set ai
set sts=4
set sta
set wrap
set lbr
set fdm=indent
set pt=<F10>
set stl=%<%F%h%m%r%h%w%y\ %{&ff}\ %{strftime(\"%d/%m/%Y-%H:%M\")}%=\ %c%V\ %l\,%L\ %P
filetype plugin on
map <F12> <Esc>:!ctags -R --sort=yes --c++-kinds=+p --fields=+iaS --extra=+q *<CR>
map <C-J> :bn<CR>
map <C-K> :bp<CR>
map <F7> <Esc>:setl spell spelllang=en_gb<CR>
map <F2> <Esc>:mks! ~/.vim/sessions/
map <leader>t <Esc>:tabe 
map <leader>e <Esc>:e 
nnoremap <silent> <F8> :TlistToggle<CR>
let Tlist_Inc_Winwidth = 0

" MiniBuf Explorer settings
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1 

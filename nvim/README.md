# My Neovim configuration
## Generated from [Jasvim](https://github.com/amirrezaask/jasvim)
## How to install
### Linux & macOS
```bash
bash <(curl -s https://raw.githubusercontent.com/amirrezaask/jasvim/master/install.sh)
```
**Note** : remember that if installation stuck due to network problems just exit and re enter and it will continue were it failed,
in my country due to network problems this happens a lot to me.

## Features
- LSP + Automatic install of LSP servers
- Tree file explorer using `nvim-tree`
- `Telescope` fuzzy finder + `Fzf` fuzzy finder
- Tmux integration
- Terminal emulator using `toggleterm`


## Why another distro when we have awesome ones like [AstroNvim](https://astronvim.github.io/) or [LunarVim](https://www.lunarvim.org/) ?
Most vim and also emacs distros will try to mimic the experience and ideas from tools like VSCode or Intelij IDEs and I'm not saying
this is wrong or anything but I think both Vim/Neovim and Emacs are different and need different thinking and maybe the audience of these
tools are not same as out of the box tools I mentioned. I think Vim/Neovim and Emacs are tools to create your own personalized developer environment
and not necesarily a conventional IDE with the same experience, maybe at start they seem hard or unproductive but I believe from my own experience and many other
people from the community of these tools, after few weeks of customizing you will feel the magic and ease of being able to customize everything to your liking
and you will feel more powerful.

## Why JasVim is different ?
JasVim is designed to be a simpler start for someone coming to neovim and yet it's not trying to abstract everything away. Flow of things is pretty clear and obvious
,there is no hidden logic and everything starts at `init.lua` and you can easily follow everything from there and start customizing anything you want, it's more like a 
a simple framework to start with and provide some sane defaults and also plugins that are defacto standards.

## Use as a template
This project is designed to be used not as a seperate distro but as a template to build your own configuration from it,
to do so use `Use this template` button and create a new repo from this template to hold your own configuration in it

## Jasvim structure
### Init flow
JasVim utilizes `packer.nvim` and uses it's declarative syntax for most plugins configurations.
In short, jasvim `init.lua` does following:
- Load `core` modules, which will load basic functionalities and plugins that are in `lua/core/*.lua`.
- Scan `lua/langs` and `lua/plugins` for lua files and load them all.
- Finally it will call `:PackerInstall` command which will install all missing plugins.
- That's it

### Plugins
All plugin configurations are in `lua/plugins/<plugin name>.lua`.

### Languages
All language configurations are in `lua/langs/<language name>.lua`.

## API
JasVim has a simple function which is a tweaked version of `packer`.use function,
so you can always refer to [packer documentation](https://github.com/wbthomason/packer.nvim#specifying-plugins) for it's keys and how to use it.
for every plugin specific configuration we use this function to install and configure that plugin,
but also in some scenarios like registering autocmd for autoformat feature we just do it in the root
of module.<br>
for example:
```lua
plugin {
  "ray-x/go.nvim",
  requires = { "nvim-lua/plenary.nvim" },
  config = function()
    require("go").setup()
    vim.api.nvim_create_autocmd("BufEnter", {
      pattern = "*.go",
      group = _G.go_group,
      callback = function(meta)
        buf_nnoremap(meta.buffer, "<leader>lat", "<cmd>GoAddTag<CR>", { remap = true })
        buf_nnoremap(meta.buffer, "<leader>lrt", "<cmd>GoRmTag<CR>", { remap = true })
        buf_nnoremap(meta.buffer, "<leader>lfs", "<cmd>GoFillStruct<CR>", { remap = true })
      end,
    })
  end,
}
```


## Themes
There is a list of all themes that are installed in this template and there is a `Theme` command to switch between them
and see which one you want and set it in your `init.lua`

### catppuccin
![Default Theme: catppuccin](https://raw.github.com/amirrezaask/jasvim/master/screenshots/catppuccin.png)

### catppuccin-mocha
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/catppuccin-mocha.png)

### catppuccin-macchiato
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/catppuccin-macchiato.png)

### catppuccin-latte
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/catppuccin-latte.png)

### catppuccin-frappe
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/catppuccin-frappe.png)

### dracula
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/dracula.png)

### gruvbox
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/gruvbox.png)

### monkai
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/monokai.png)

### moonfly
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/moonfly.png)

### nightfly
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/nightfly.png)

### nightfox 
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/nightfox.png)

### onedark 
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/onedark.png)

### rose-pine 
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/rose-pine.png)

### sonokai 
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/sonokai.png)

### tokyodark 
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/tokyodark.png)

### tokyonight-day 
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/tokyonight-day.png)

### tokyonight-moon 
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/tokyonight-moon.png)

### tokyonight-night 
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/tokyonight-night.png)

### tokyonight-storm 
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/tokyonight-storm.png)

### tokyonight 
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/tokyonight.png)

### ayu-mirage
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/ayu-mirage.png)

### ayu-light
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/ayu-light.png)

### ayu-dark
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/ayu-dark.png)

## Screenshots
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/0.png)
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/1.png)
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/2.png)
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/3.png)
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/4.png)
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/5.png)
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/6.png)
![Screenshot](https://raw.github.com/amirrezaask/jasvim/master/screenshots/7.png)


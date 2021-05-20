vim.map {
  ['n ,b'] = require('blame').blame,
  ['n ,c'] = require('blame').clear
}

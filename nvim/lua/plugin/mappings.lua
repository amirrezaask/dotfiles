local nvim = require('amirrezaask.nvim')

nvim.map {
  ['n ,b'] = require('blame').blame,
  ['n ,c'] = require('blame').clear
}

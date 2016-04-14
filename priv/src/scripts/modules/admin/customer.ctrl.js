define([
  './controller',
  './views/customer.view',
],

function(Controller, CustomerView) {
  'use strict';

  var customerCtrl = Controller.extend({

    index: function() {
      var that = this;
      var view = new CustomerView();
      that.layout.showChildView('content', view);
    },
  });

  return customerCtrl;
});

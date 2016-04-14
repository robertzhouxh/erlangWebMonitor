define([
  './controller',
  './views/openresty.view',
],

function(Controller, OpenrestyView) {
  'use strict';

  var openrestyCtrl = Controller.extend({

    index: function() {
      this.layout.showChildView('content', new OpenrestyView());
    },
  });

  return openrestyCtrl;
});

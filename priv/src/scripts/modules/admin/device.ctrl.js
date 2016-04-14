define([
  './controller',
  './views/device.view',
  './models/device.collection',
],

function(Controller, DeviceView, Device) {
  'use strict';

  var deviceCtrl = Controller.extend({

    index: function() {
      var that = this;
      var view = new DeviceView({model: new Device()});
      that.layout.showChildView('content', view);
    },
  });

  return deviceCtrl;
});

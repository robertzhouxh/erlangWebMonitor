define([
  './models/user.model',
  '../common/index.view',
  './views/login.view',
],

function(User, RootView, LoginView) {
  'use strict';

  var controller = function() {};
  controller.prototype.index = function() {
    var root = new RootView();
    root.$el.removeClass().addClass('hold-transition login-page');

    var user = new User();
    root.showChildView('wrapper', new LoginView({model: user}), {});
  };

  return controller;
});

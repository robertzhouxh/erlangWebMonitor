define([
  './views/layout.view',
  './views/header.view',
  './views/sidebar.view',
  './views/footer.view',
],

function(LayoutView, HeaderView, SidebarView, FooterView) {
  'use strict';

  function Controller() {
    var instance;

    //  overwrite the contructor
    /* eslint no-func-assign: 0 */
    Controller = function Controller() {
      return instance;
    };

        // carry over the prototype properties.
    Controller.prototype = this;

    instance = new Controller();

        // reset the contructor pointer
    instance.constructor = Controller;

        // functionality
    var layout = new LayoutView();
    layout.setChildView({
      header: new HeaderView(),
      sidebar: new SidebarView(),
      footer: new FooterView(),
    });
    layout.render();

    instance.layout = layout;

    return instance;
  }

  Controller.extend = Marionette.extend;

  return Controller;
});

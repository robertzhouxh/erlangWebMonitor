define(['hbs!./templates/sidebar'],

function(tmplSidebar) {
  'use strict';

  var sidebarView = Marionette.CompositeView.extend({
    template: tmplSidebar,

    events: {
      'click li a': 'tree',
    },

    onRender: function() {
    },

    animationSpeed: 200,

    screenSizes: {
      xs: 480,
      sm: 768,
      md: 992,
      lg: 1200,
    },

    tree: function(e) {
      var speed = this.animationSpeed;
      var $this = $(e.currentTarget);
      var checkElement = $this.next();

      if ((checkElement.is('.treeview-menu')) && (checkElement.is(':visible'))) {
        checkElement.slideUp(speed, function() {
          checkElement.removeClass('menu-open');
        });
        checkElement.parent('li').removeClass('active');
      } else if ((checkElement.is('.treeview-menu')) && (!checkElement.is(':visible'))) {
        var parent = $this.parents('ul').first();
        var ul = parent.find('ul:visible').slideUp(speed);
        ul.removeClass('menu-open');
        var parentLi = $this.parent('li');

        checkElement.slideDown(speed, function() {
          checkElement.addClass('menu-open');
          parent.find('li.active').removeClass('active');
          parentLi.addClass('active');
              //  _this.layout.fix();
        });
      }
      if (checkElement.is('.treeview-menu')) {
        e.preventDefault();
      }
    },
  });

  return sidebarView;
});

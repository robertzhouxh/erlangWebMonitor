define(['hbs!./templates/layout'],

function(tmplLayout) {
  'use strict';

  var layoutView = Marionette.LayoutView.extend({
    el: '#wrapper',
    template: tmplLayout,

    childViews: [],

    regions: {
      header: '.main-header',
      sidebar: '.main-sidebar',
      content: '.content-wrapper',
      footer: '.main-footer',
    },

    onRender: function() {
      var view,
        that = this;

      $('body').removeClass().addClass('skin-blue sidebar-mini');
      that.$el.removeClass().addClass('wrapper');

      for (var region in that.childViews) {
        if (that.regions[region]) {
          view = that.childViews[region];
          view.setElement(that.regions[region]);
          view.render();
        }
      }

      that.fix();
      that.fixSidebar();
      $(window, '.wrapper').resize(function() {
        that.fix();
        that.fixSidebar();
      });
    },

    setChildView: function(region, view) {
      var attr;
      if (region === null || typeof region === 'object') {
        attr = region;
      } else {
        (attr = {})[region] = view;
      }

      this.childViews = _.extend(this.childViews, attr);
    },

    renderChildView: function(region, view) {
      view.setElement(this.regions[region]);
      view.render();
    },

    fix: function() {
      var neg = $('.main-header').outerHeight() + $('.main-footer').outerHeight();
      var window_height = $(window).height();
      var sidebar_height = $('.sidebar').height();
      if ($('body').hasClass('fixed')) {
        $('.content-wrapper').css('min-height', window_height - $('.main-footer').outerHeight());
      } else {
        /* eslint no-unused-vars: 0 */
        var postSetWidth;
        if (window_height >= sidebar_height) {
          $('.content-wrapper').css('min-height', window_height - neg);
          postSetWidth = window_height - neg;
        } else {
          $('.content-wrapper').css('min-height', sidebar_height);
          postSetWidth = sidebar_height;
        }
      }
    },

    fixSidebar: function() {
      if (!$('body').hasClass('fixed')) {
        if (typeof $.fn.slimScroll !== 'undefined') {
          $('.sidebar').slimScroll({destroy: true}).height('auto');
        }
        return;
      } else if (typeof $.fn.slimScroll === 'undefined' && window.console) {
        window.console.error('Error: the fixed layout requires the slimscroll plugin!');
      }

      /* eslint no-constant-condition: 0 */
      if (!true) {
        if (typeof $.fn.slimScroll !== 'undefined') {
          $('.sidebar').slimScroll({destroy: true}).height('auto');
          $('.sidebar').slimscroll({
            height: ($(window).height() - $('.main-header').height()) + 'px',
            color: 'rgba(0,0,0,0.2)',
            size: '3px',
          });
        }
      }
    },
  });

  return layoutView;
});

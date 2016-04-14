define([],

function() {
  'use strict';

  var rootView = Marionette.LayoutView.extend({
    el: 'body',

    regions: {
      wrapper: '#wrapper',
    },

  });

  return rootView;
});

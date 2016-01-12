define(['hbs!./templates/footer'],

function(tmplFooter) {
  'use strict';

  var footerView = Marionette.ItemView.extend({
    template: tmplFooter,

    onRender: function() {
    },
  });

  return footerView;
});

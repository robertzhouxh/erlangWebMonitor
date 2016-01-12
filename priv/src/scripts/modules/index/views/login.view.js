define(['hbs!./templates/index'],

function(tmplIndex) {
  'use strict';

  var loginView = Marionette.ItemView.extend({
    template: tmplIndex,
    tagName: 'div',
    className: 'login-box',

    initialize: function() {
      this.listenTo(this.model, 'change', function() {
        this.render();
      });
    },

    events: {
      'submit form': 'onSubmit',
    },

    onSubmit: function(e) {
      e.preventDefault();
      var username = $('input[name=\'username\']').val();
      var password = $('input[name=\'password\']').val();
      this.model.unset('msg');
      /* eslint no-unused-vars: 0 */
      this.model.login(username, password, {
        success: function(model, respone, options) {
          Backbone.history.navigate('admin/customer', {trigger: true});
        },
        error: function(model, respone, options) {
          model.set('msg', respone.status + ': ' + respone.statusText);
        },
      });
    },
  });

  return loginView;
});

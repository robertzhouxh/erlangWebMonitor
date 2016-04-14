define([
  'hbs!./templates/customer',
  '../models/customer.collection',
  '../models/online.collection',
  '../../common/chart.lib',
],

function(tmplCustomer, Customer, Online, Chart) {
  'use strict';

  var UsersView = Marionette.ItemView.extend({
    el: '.users',
    ui: {
      registOnPC: '#pc_register',
      registOnAPP: '#phone_register',
      registLine: '#user_register',
    },

    initialize: function() {
      this.listenTo(this.model, 'change', this.sync);
      this.listenTo(this.model, 'error', syncError);

      this.model.fetch();
    },

    /* eslint no-unused-vars: 0 */
    /* eslint new-cap: 0 */
    sync: function(model, options) {
      $(this.ui.registOnPC).text(model.get('pc_total'));
      $(this.ui.registOnAPP).text(model.get('app_total'));

      var ctx = $(this.ui.registLine).get(0).getContext('2d');
      var bar = Chart.Bar(ctx, model.get('days_users'), {num: 15});
    },
  });

  var OnlineView = Marionette.ItemView.extend({
    el: '.online',
    ui: {
      onlinePC: '#pc_online',
      onlineAPP: '#phone_online',
      onlineLine: '#user_online',
    },

    initialize: function() {
      this.listenTo(this.model, 'change', this.sync);
      this.model.fetch();
    },

    sync: function(model, options) {
      $(this.ui.onlinePC).text(model.get('total_browser'));
      $(this.ui.onlineAPP).text(model.get('total_app'));

      var ctx = $(this.ui.onlineLine).get(0).getContext('2d');
      var bar = Chart.Bar(ctx, model.get('users'), {num: 15, groupby: 'login_at'});
    },
  });

  var customerView = Marionette.LayoutView.extend({
    template: tmplCustomer,
    templateHelpers: {
      title: '用户统计',
      subTitle: 'Dashboard',
    },

    regions: {
      users: '.users',
      online: '.online',
    },

    /* eslint no-new: 0 */
    onRender: function() {
      var customer = new Customer();
      var online = new Online();

      new UsersView({model: customer});
      new OnlineView({model: online});
    },
  });

  function syncError(model, resp, options) {
    Backbone.history.navigate('', {trigger: true});
  }

  return customerView;
});

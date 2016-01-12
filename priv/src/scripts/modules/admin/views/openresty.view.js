define([
  'hbs!./templates/openresty',
  'hbs!./templates/openresty/composite',
  'hbs!./templates/openresty/item',
  '../../admin/models/log.collection',
  'datatables.bootstrap',
],

function(tmplOpenresty, tmplComposite, tmplItem, LogCollection) {
  'use strict';

  var LogItemView = Marionette.ItemView.extend({
    tagName: 'tr',
    template: tmplItem,
    templateHelpers: function() {
      var severity = '';
      switch (this.model.get('SEVERITY_NUM')) {
        case 0:
        case 1:
        case 2:
        case 3:
          severity = 'label-danger';
          break;
        case 4:
          severity = 'label-warning';
          break;
        case 5:
        case 6:
          severity = 'label-info';
          break;
        case 7:
          severity = 'label-primary';
          break;
        default:
          severity = 'label-primary';
          break;
      }

      return {level: severity};
    },
  });

  var LogCompositeView = Marionette.CompositeView.extend({
    tagName: 'div',
    className: 'col-xs-12',
    template: tmplComposite,
    childView: LogItemView,
    childViewContainer: 'tbody',
    ui: {
      table: '.table',
    },

    initialize: function() {
      var that = this;
      this.listenTo(this.collection, 'error', syncError);
      that.listenTo(that.collection, 'update', function() {
        that.render();
      });
    },

    onRender: function() {
      /* eslint new-cap: 0 */
      this.ui.table.DataTable();
    },
  });

  var openrestyView = Marionette.LayoutView.extend({
    template: tmplOpenresty,
    templateHelpers: {
      title: '系统日志',
      subTitle: 'OpenResty',
      description: 'Web server log',
    },

    regions: {
      content: '.content .row',
    },

    onShow: function() {
      var logs = new LogCollection();
      this.showChildView('content', new LogCompositeView({collection: logs}));
      logs.fetch();
    },
  });

  /* eslint no-unused-vars: 0 */
  function syncError(model, resp, options) {
    Backbone.history.navigate('', {trigger: true});
  }

  return openrestyView;
});

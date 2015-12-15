define([
    "hbs!./templates/device",
    "../../common/chart.lib"
],

function(tmplDevice, Chart){
    'use strict';

    var deviceView = Marionette.LayoutView.extend({
        template: tmplDevice,
        templateHelpers: {
            title: "设备统计",
            subTitle: "Dashboard",
        },

        ui: {
            'numTotal': "#total_register",
            'numPublic': "#total_public",
            'lineTotal': "#dev_register",
            'linePublic': "#dev_public"
        },

        initialize: function() {
            this.listenTo(this.model, 'change', this.sync);
            this.listenTo(this.model, 'error', syncError);
            this.model.fetch();
        },

        sync: function(model, options) {
            var ctx = $(this.ui.lineTotal).get(0).getContext("2d");
            var bar = Chart.Bar(ctx, model.get('days_new_devs'), {groupby: 'created_at', num: 15});

            ctx = $(this.ui.linePublic).get(0).getContext("2d");
            bar = Chart.Bar(ctx, model.get('days_public_devs'), {groupby: 'created_at', num: 15});

            $(this.ui.numTotal).text(model.get("total_devs"));
            $(this.ui.numPublic).text(model.get("public_devs"));
        },
    });

    function syncError(model, resp, options) {
        Backbone.history.navigate('', {trigger: true});
    }

    return deviceView;
});

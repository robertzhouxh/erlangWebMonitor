define([
    "hbs!./templates/device",
    "../../common/chart.lib"
],

function(tmplDevice, Chart){
    'use strict';

    var deviceView = Marionette.LayoutView.extend({
        template: tmplDevice,
        templateHelpers: {
            title: "Devices",
            subTitle: "Dashboard",
        },

        ui: {
            'numTotal': "#total_register",
            'numPublic': "#total_public",
            'lineTotal': "#dev_register",
            'linePublic': "#dev_public"
        },

        initialize: function() {
            this.listenTo(this.model, 'sync', this.sync);
            this.model.fetch();
        },

        sync: function(model, resp, options) {
            var ctx = $(this.ui.lineTotal).get(0).getContext("2d");
            var line = Chart.Line(ctx, model.get('days_new_devs'), {groupby: 'created_at', num: 15});

            ctx = $(this.ui.linePublic).get(0).getContext("2d");
            line = Chart.Line(ctx, model.get('days_public_devs'), {groupby: 'created_at', num: 15});

            $(this.ui.numTotal).text(model.get("total_devs"));
            $(this.ui.numPublic).text(model.get("public_devs"));
        },
    });

    return deviceView;
});

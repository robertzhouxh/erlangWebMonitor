define([
    'hbs!./templates/openresty',
    //'hbs!./templates/openresty/composite',
    //'hbs!./templates/openresty/item',
    'datatables.bootstrap',
],

function(tmplOpenresty){
    'use strict';

    //var LogItemView = Marionette.ItemView.extend({
        //tagName: 'tr',
        //template: tmplItem,
    //});

    //var LogCompositeView = Marionette.CompositeView.extend({
        //tagName: 'div',
        //className: 'col-xs-12',
        //template: tmplComposite,
        //childView: LogItemView,
        //childViewContainer: 'tbody',
        //ui: {
            //table: '.table',
        //},
    //});

    var openrestyView = Marionette.LayoutView.extend({
        template: tmplOpenresty,
        templateHelpers: {
            title: '系统日志',
            subTitle: 'OpenResty',
            description: 'Web server log',
        },

        ui: {
            table: '.table',
        },

        onShow: function() {
            this.ui.table.DataTable({
                "ajax": "v2/logs"
            });
        },
    });

    return openrestyView;
});

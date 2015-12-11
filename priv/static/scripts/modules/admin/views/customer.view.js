define(["hbs!./templates/customer"],

function(tmplCustomer){
    'use strict';

    var customerView = Marionette.LayoutView.extend({
        template: tmplCustomer,
        templateHelpers: {
            title: "User Online",
            subTitle: "Dashboard",
        },

        initialize: function() {
        },

        onRender: function() {

            this.listenTo(this.model, 'change', this.refresh);
            this.model.fetch();
        },

        refresh: function(model, options) {
            console.log(arguments);
        },
    });

    return customerView;
});

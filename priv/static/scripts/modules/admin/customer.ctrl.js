define([
    './controller',
    './models/customer.model',
    './views/customer.view',
],

function(Controller, CustomerModel, CustomerView){
    'use strict';

    var customerCtrl = Controller.extend({

        index: function(){
            var that = this;
            var view = new CustomerView({model: new CustomerModel()});
            that.layout.showChildView('content', view);
        },
    });

    return customerCtrl;
});

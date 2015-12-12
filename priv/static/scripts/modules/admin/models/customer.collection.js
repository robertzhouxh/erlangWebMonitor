define([
],

function(){
    'use strict';

    var customerModel = Backbone.Model.extend({
        urlRoot: 'v2/users',

        parse: function(resp, options) {
        }
    });

    var customerCollection = Backbone.Collection.extend({
        url: 'v2/users',
        model: customerModel,
    });

    return customerModel;
});

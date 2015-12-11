define([
],

function(){
    'use strict';

    var customerModel = Backbone.Model.extend({
        urlRoot: 'v2/users',
    });

    return customerModel;
});

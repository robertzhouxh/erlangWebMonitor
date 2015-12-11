define([
    'marionette'
],

function(Marionette){
    'use strict';

    var app = new Marionette.Application();

    app.on("before:start", function() {
    });

    app.on("start", function() {
        console.log('application startup.');

        if(Backbone.history) {
            Backbone.history.start({pushState: false});
        }
    });

    return app;
});

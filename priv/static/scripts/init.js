(function() {
    'use strict';

    require.config({
        baseUrl: 'scripts',
        paths: {
            jquery: "../bower_components/jquery/dist/jquery",
            lodash: "../bower_components/lodash/lodash",
            underscore: "../bower_components/underscore/underscore",

            backbone: "../bower_components/backbone/backbone",
            marionette: "../bower_components/backbone.marionette/lib/core/backbone.marionette",
            "backbone.babysitter": "../bower_components/backbone.babysitter/lib/backbone.babysitter",
            "backbone.wreqr": "../bower_components/backbone.wreqr/lib/backbone.wreqr",

            hbs: "../bower_components/require-handlebars-plugin/hbs",
            handlebars: "../bower_components/require-handlebars-plugin/hbs/handlebars.runtime",

            chartjs: "../bower_components/Chart.js/Chart",
            "datatables.net": "../bower_components/datatables.net/js/jquery.dataTables",
            "datatables.bootstrap": "../bower_components/datatables/media/js/dataTables.bootstrap",
        },
        shim: {
            jquery: {
                exports: "$"
            },
            lodash: {
                exports: "_"
            },
            backbone: {
                deps: ["jquery", "lodash"],
                exports: "Backbone"
            },
            "datatables.net": ["jquery"],
            "datatables.bootstrap": ["datatables.net"],
        },
        hbs: {
            helpers: true,
            templateExtension: 'hbs',
            handlebarsPath: 'handlebars',
            partialsUrl: ''
        },

        deps: ["startup"]
    });

})();

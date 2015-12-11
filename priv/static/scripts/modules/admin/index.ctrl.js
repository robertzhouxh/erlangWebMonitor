define([
    './controller',
],

function(Controller){
    'use strict';

    var dashboardCtrl = Controller.extend({

        index: function() {
            console.log('dashboardCtrl');
        },
    });

    return dashboardCtrl;
});

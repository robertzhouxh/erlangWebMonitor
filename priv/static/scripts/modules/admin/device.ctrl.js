define([
    './controller',
],

function(Controller){
    'use strict';

    var deviceCtrl = Controller.extend({

        index: function() {
            console.log('deviceCtrl');
        },
    });

    return deviceCtrl;
});

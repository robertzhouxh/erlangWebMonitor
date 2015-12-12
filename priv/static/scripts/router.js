define([
    'marionette'
],

function(Marionette) {
    'use strict';

    var _prefix = 'modules/';

    var Controller = {
        "dispatch": doDispatch,
        "default": doDefault
    };

    var Router = Marionette.AppRouter.extend({});

    var router = new Router({
        controller: Controller,
    });

    router.route(/^(.*?)$/, doDispatch);
    router.route("", doDefault);

    //router.on('route', function(name, args){
    //});

    function doDispatch(hash) {
        hash = hash || 'index';
        var uri = hash.replace(/(^\/)|(\/$)/g, '').split('/');

        for(var i=uri.length; i<3; i++) {
            uri.push('index');
        }

        var path = _prefix + uri.slice(0,2).join('/') + '.ctrl';
        require([path], function(Controller) {
            var controller = new Controller();
            controller[uri[2]](uri.slice(3));
        });
    }

    function doDefault() {
        var path = _prefix + 'index/index.ctrl';
        require([path], function(Controller) {
            var action = 'index';
            var controller = new Controller();
            controller[action]();
        });
    }

    return router;
});

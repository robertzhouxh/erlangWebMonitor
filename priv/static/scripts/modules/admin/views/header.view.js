define(["hbs!./templates/header"],

function(tmplHeader){
    'use strict';

    var headerView = Marionette.CompositeView.extend({
        template: tmplHeader,

        events: {
            'click .sidebar-toggle': 'offcanvas',
        },

        onRender: function() {
        },

        offcanvas: function(e) {
            e.preventDefault();
        },
    });

    return headerView;
});

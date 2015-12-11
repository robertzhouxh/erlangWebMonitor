define(["router", "hbs!./templates/index"],

function(router, tmplIndex){
    'use strict';

    var loginView = Marionette.ItemView.extend({
        template: tmplIndex,
        tagName: 'div',
        className: 'login-box',

        events: {
            'submit form': 'onSubmit',
        },

        onSubmit: onSubmit,
        onShow: onShow,
    });

    function onSubmit(e) {
        e.preventDefault();
        var username = $("input[name='username']").val();
        var password = $("input[name='password']").val();
        this.model.login(username, password, {
            success: function(model, respone, options) {
                router.navigate("admin/customer", {trigger: true});
            },
            error: function(model, respone, options) {
            }
        });
    }

    function onShow() {
    }

    return loginView;
});

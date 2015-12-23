define([
],

function(){
    'use strict';

    var userModel = Backbone.Model.extend({
        urlRoot: 'v2/login',

        defaults: {
            username: 'admin',
            password: 'pass'
        },

        login: function(username, password, options) {
            username = username || this.username;
            password = password || this.password;
            this.save({
                username: username,
                password: password
            }, options);
        },
    });

    //function login(username, password, options) {
        //username = username || this.username;
        //password = password || this.password;
        //this.save({
            //username: username,
            //password: password
        //}, options);
    //}

    return userModel;
});

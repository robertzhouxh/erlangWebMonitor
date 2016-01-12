/* eslint no-unused-vars: 0 */
define([
],

function() {
  'use strict';

  var onlineModel = Backbone.Model.extend({
    urlRoot: 'v2/online',
  });

  var onlineCollection = Backbone.Collection.extend({
    url: 'v2/online',
    model: onlineModel,
  });

  return onlineModel;
});

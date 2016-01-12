/* eslint no-unused-vars: 0 */
define([
],

function() {
  'use strict';

  var deviceModel = Backbone.Model.extend({
    urlRoot: 'v2/devices',
  });

  var deviceCollection = Backbone.Collection.extend({
    url: 'v2/devices',
    model: deviceModel,
  });

  return deviceModel;
});

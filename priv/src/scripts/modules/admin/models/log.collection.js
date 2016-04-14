define([
],

function() {
  'use strict';

  var logModel = Backbone.Model.extend({
    urlRoot: 'v2/logs',
  });

  var logCollection = Backbone.Collection.extend({
    url: 'v2/logs',
    model: logModel,
  });

  return logCollection;
});

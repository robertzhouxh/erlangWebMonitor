define([],

function() {
  var options = {
    // can be develop, test or production
    enviroment: 'develop',
  };

  var develop = {};

  var test = {};

  var production = {};

  var result = {
    develop: develop,
    test: test,
    production: production,
  };

  return result[options.enviroment];
});

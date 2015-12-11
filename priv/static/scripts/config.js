define([],

function() {

    var options = {
        enviroment: "develop", // can be develop, test or production
    };

    var develop = {};

    var test = {};

    var production = {};

    var result = {
        "develop": develop,
        "test": test,
        "production": production,
    };

    return result[options.enviroment];
});

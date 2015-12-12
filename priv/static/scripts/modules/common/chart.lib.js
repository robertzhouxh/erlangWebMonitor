define([
    "chartjs",
],

function(Chart) {

    function drawLine(ctx, data, options) {
        var Chartjs = Chart.noConflict(),
            chartData;

            options = _.extend({
                num: 30,
                groupby: "regdate",
            }, options);
        chartData = getChartData(data, options);
        return new Chartjs(ctx).Line(chartData, {
            showScale: true,
            scaleShowGridLines: false,
            scaleGridLineColor: "rgba(0,0,0,.05)",
            scaleGridLineWidth: 1,
            scaleShowHorizontalLines: true,
            scaleShowVerticalLines: true,
            bezierCurve: true,
            bezierCurveTension: 0.3,
            pointDot: false,
            pointDotRadius: 4,
            pointDotStrokeWidth: 1,
            pointHitDetectionRadius: 20,
            datasetStroke: true,
            datasetStrokeWidth: 2,
            datasetFill: true,
            legendTemplate: "<ul class=\"<%=name.toLowerCase()%>-legend\"><% for (var i=0; i<datasets.length; i++){%><li><span style=\"background-color:<%=datasets[i].lineColor%>\"></span><%=datasets[i].label%></li><%}%></ul>",
            maintainAspectRatio: true,
            responsive: true
        });
    }

    function getChartData(collection, options) {
         var label,
             labels = [],
             today,
             current,
             groupby,
             data = [];

        today = new Date();
        today.setDate(today.getDate()+1);
        today.setHours(0);
        today.setMinutes(0);
        today.setSeconds(0);
        today.setMilliseconds(0);

        groupby = _.groupBy(collection, function(model){
            current = new Date(parseInt(model[options.groupby]) * 1000);
            return (current.getMonth() + 1) + '/' + current.getDate();
        });

        for(var i=0; i<options.num; i++) {
            today.setDate(today.getDate() - 1);
            label = (today.getMonth()+1) + '/' + today.getDate();
            labels.unshift(label);
            if(groupby[label] === undefined){
                 data.unshift(0);
            }
            else {
                data.unshift(groupby[label].length);
            }
        }

        return {
            labels: labels,
            datasets: [{
                fillColor: "rgba(220,220,220,0.2)",
                strokeColor: "rgba(220,220,220,1)",
                pointColor: "rgba(220,220,220,1)",
                pointStrokeColor: "#fff",
                pointHighlightFill: "#fff",
                pointHighlightStroke: "rgba(220,220,220,1)",
                data: data
            }]
        };
    }

    return {
        Line: drawLine,
    };
});

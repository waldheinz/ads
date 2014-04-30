
function appendStatus(container) {
    container.append($('<h1>').text('Status'));
    container.append($('<h2>').text('Data Stores'));
    
    container.append($('<h3>').text('CHK Store'));
    var chkStatus = $('<p>');
    container.append(chkStatus);
    appendStoreStatus("api/status/store/chk", chkStatus);
    
    container.append($('<h3>').text('SSK Store'));
    var sskStatus = $('<p>');
    container.append(sskStatus);
    
    appendStoreStatus("api/status/store/ssk", sskStatus);
}

function appendStoreStatus(url, container) {
    $.getJSON(url).then(function (json) {
        container.append(makeHist(json.histogram));
        
        var used = 0;
        
        for (var i=0; i < json.histogram.length; i++) {
            used += json.histogram[i][1];
        }
        var pct = 100 * used / json.capacity;
        
        var bar = $('<div>')
                .addClass('progress-bar')
                .css('width', pct + '%');
            
        container.append($('<div>').addClass('progress')
                .append(bar));
    });
}

function makeHist(data) {
    var margin = {top: 10, right: 30, bottom: 30, left: 30},
    width = 452 + margin.left + margin.right,
            height = 128 - margin.top - margin.bottom;

    var x = d3.scale.linear()
            .domain([0, 1])
            .range([0, width]);

    var y = d3.scale.linear()
            .domain([0, d3.max(data, function(d) {
                    return d[1];
                })])
            .range([height, 0]);

    var xAxis = d3.svg.axis()
            .scale(x)
            .orient("bottom");
    
    var result = document.createElementNS(d3.ns.prefix.svg, 'svg');
    var svg = d3.select(result);
    
    svg.attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom)
            .append("g")
            .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var bar = svg.selectAll(".bar")
            .data(data)
            .enter().append("g")
            .attr("class", "bar")
            .attr("transform", function(d) {
                return "translate(" + x(d[0] / 256) + "," + y(d[1]) + ")";
            });

    bar.append("rect")
            .attr("x", 1)
            .attr("width", 1)
            .attr("height", function(d) {
                return height - y(d[1]);
            });

    svg.append("g")
            .attr("class", "x axis")
            .attr("transform", "translate(0," + height + ")")
            .call(xAxis);
    
    return result;
}

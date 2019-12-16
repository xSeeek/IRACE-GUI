function getOptionsPDF()
{
    var numberOfIterations = $('#numberOfIterations').html();
    var options = [];
    var index = 0;

    if(!$("#dontIncludeCandidates").prop("checked"))
    {
        var iterations = [numberOfIterations - 1, numberOfIterations];
        var allIterations = false;
        if($("#allIterationCandidates").prop("checked"))
        {
            iterations = Array.apply(null, {length: numberOfIterations}).map(function(value, index){
                return index + 1;
            });
            allIterations = true;
        }
        var data = {"candidates": true, "candidatesAllIterations": allIterations, "candidatesIterations": iterations};
        options[index] = data;
    }
    else
        options[index] = {"candidates": false};

    index++;

    if(!$("#dontIncludePerfomance").prop("checked"))
    {
        var iterations = [numberOfIterations];
        var allIterations = false;
        if($("#allIterationsPerfomance").prop("checked"))
        {
            iterations = Array.apply(null, {length: numberOfIterations}).map(function(value, index){
                return index + 1;
            });
            allIterations = true;
        }
        var data = {"perfomance": true, "perfomanceAllIterations": allIterations, "perfomanceIterations": iterations};
        options[index] = data;
    }
    else
        options[index] = {"perfomance": false};

    index++;

    var values = $("#selectParametersPDF").val();
    var parameters = values;

    if($("#includeDetails").prop("checked") && parameters.length != 0)
    {
        var data = {"details": true, "parameters": parameters};
        options[index] = data;
    }
    else
        options[index] = {"details": false};

    generatePDF(options);
}

$(function() {
    enableAllIterationsCandidates();
    enableAllIterationsPerfomance();
    showButtonParameters();
    $("#dontIncludeCandidates").click(enableAllIterationsCandidates);
    $("#dontIncludePerfomance").click(enableAllIterationsPerfomance);
    $("#includeDetails").click(showButtonParameters);
});
  
function enableAllIterationsCandidates() {
    if (!this.checked) {
        $("#allIterationCandidates").removeAttr("disabled");
    } else {
        $("#allIterationCandidates").attr("disabled", true);
    }
}

function enableAllIterationsPerfomance() {
    if (!this.checked) {
        $("#allIterationsPerfomance").removeAttr("disabled");
    } else {
        $("#allIterationsPerfomance").attr("disabled", true);
    }
}

function showButtonParameters() {
    if (this.checked) {
        $("#buttonParametersPDF").removeAttr("hidden");
    } else {
        $("#buttonParametersPDF").attr("hidden", true);
    }
}

async function generatePDF(options)
{
    showLoading();

    var doc = new jsPDF("portrait", "mm", "letter");
    var date = new Date();
    var index = 0;

    // HEADER
    doc.setFont("times", "bold");
    doc.setFontSize(16);
    doc.text(72, 20, 'IRACE Autogenerated report');

    doc.setFont("times", "normal");
    doc.setFontSize(11);
    doc.text(20, 40, 'Date of the report: ' + date);

    // SUMMARY
    updateTextStatus("Generating Summary");
    await appendSummary(doc);

    // BEST CONFIGURATION
    updateTextStatus("Generating Best Configuration");
    await appendBestConfiguration(doc);

    // CANDIDATES
    if(options[index]['candidates'] != false)
    {
        showLoading();
        await updateTextStatus("Generating Candidates Section");
        var iterations = options[index]['candidatesIterations'];
        var lastTwo = 'Last two iterations';
        await appendCandidates(doc, [options[index]['candidatesIterations'][options[index]['candidatesIterations'].length-2], options[index]['candidatesIterations'][options[index]['candidatesIterations'].length-1]], lastTwo);
        if(options[index]['candidatesAllIterations'] == true)
        {
            var iterations = options[index]['candidatesIterations'];
            var lastTwo = 'All iterations';
            await appendCandidates(doc, iterations, lastTwo);
        }
    }
    
    index++;

    // PERFOMANCE
    if(options[index]['perfomance'] != false)
    {
        showLoading();
        updateTextStatus("Generating Perfomance Section");
        var iterations = options[index]['perfomanceIterations'][options[index]['perfomanceIterations'].length-1];
        var textIteration = 'Last iteration';
        await appendPerfomance(doc, iterations, textIteration, true)
        if(options[index]['perfomanceAllIterations'] == true)
        {
            var iterations = options[index]['perfomanceIterations'];
            for(var i = iterations.length - 2; i >= 0; i--)
            {
                if(i == 0)
                    var textIteration = 'Iteration ' + iterations[i];
                else
                    var textIteration = 'Iterations ' + iterations[i] + " - " + iterations[i - 1];
                await appendPerfomance(doc, iterations[i], textIteration, false)
                i--;
            }
        }
    }

    index++;

    // DETAILS
    if(options[index]['details'])
    {
        showLoading();
        updateTextStatus("Generating Details by Iteration");
        await appendDetailsSectionPDF(doc, options[index]['parameters']);
    }

    // CUSTOM SECTIONS
    showLoading();
    updateTextStatus("Generating Custom Sections");
    await appendCustomSectionsPDF(doc);

    var element = document.getElementsByTagName("iframe"), index;
    for (index = element.length - 1; index >= 0; index--) {
        element[index].parentNode.removeChild(element[index]);
    }

    doc.setProperties({
        title: 'IRACE Report',	
        author: 'IRACE-GUI',
        creator: 'IRACE-GUI'
    });

    console.log('Save PDF');
    doc.save('IRACE' + date.getTime() + '.pdf');

    swal.close();
    confirmMessage("PDF generated successfully");

    return;
}

/* MAIN FUNCTIONS FOR THE GENERATION OF THE PDF */

async function appendSummary(doc)
{
    doc.setFont("times", "bold");
    doc.setFontSize(14);
    doc.text(20, 55, 'Summary')
    doc.line(19, 56, 42, 56);
    doc.setFont("times", "normal");
    doc.setFontSize(11);
    doc.autoTable({
        startY: 60,
        html: '#tablaSummary1',
        styles: {overflow: 'hidden'},
        margin: {right: 125},
        theme: 'plain'
    });
    doc.autoTable({
        startY: 60,
        html: '#tablaSummary2',
        styles: {overflow: 'hidden'},
        margin: {left: 125},
        theme: 'plain'
    });
}

async function appendBestConfiguration(doc)
{
    doc.setFont("times", "bold");
    doc.setFontSize(14);
    doc.text(20, 130, 'Best Configuration')
    doc.line(19, 131, 63, 131);
    doc.setFont("times", "normal");
    doc.setFontSize(8);
    source = $('#boxPlotBestConfiguration').children('img')[0].src;

    await loadImage(source).then(formatedImage =>{
        doc.addImage(formatedImage, 119, 140, 60, 70);
        console.log('Add image: Boxplot best-configuration')
    })

    var bestConfiguration = formatBestConfiguration();
    var yLine = 140;
    var pageHeight = 257;
    for(i = 0; i < bestConfiguration.length; i++)
    {
        doc.setFont("times", "normal");
        doc.setFontSize(8);
        doc.text(20, yLine, bestConfiguration[i])
        if(i >= 20 && i < (bestConfiguration.length-1))
        {
            doc.text(125, yLine, bestConfiguration[i+1]);
            i++;
        }
        yLine += 4
        if(pageHeight <=  yLine)
        {
            doc.setFont("times", "bold");
            doc.setFontSize(16);
            doc.text(72, 20, 'IRACE Autogenerated report');
            yLine = 30;
            pageHeight = 265;
            doc.addPage();
            doc.setPage(doc.internal.getNumberOfPages());
        }
    }
}

async function appendCandidates(doc, iterations, lastTwo)
{
    doc.addPage();
    doc.setPage(doc.internal.getNumberOfPages());
    doc.setFont("times", "bold");
    doc.setFontSize(16);
    doc.text(72, 20, 'IRACE Autogenerated report');

    doc.setFont("times", "bold");
    doc.setFontSize(14);
    doc.text(20, 35, 'Candidates')
    doc.line(19, 36, 55, 36);
    doc.setFontSize(11);
    doc.text(20, 40, lastTwo)
    doc.setFont("times", "bold");
    doc.setFontSize(9);
    doc.text(50, 50, 'Frequency Plots')
    doc.text(142, 50, 'Parallel Coordinates Plots')

    Shiny.onInputChange("requestPlottingCandidates", iterations);
    await waitForData(1).then(async plots =>{
        var yLine = 55;
        var imgHeight = 95;
        var pageHeight = 257;
        if(plots['frequency'][0].length == 1 || plots['parallel'][0].length == 1)
        {
            await loadImage(plots['frequency']).then(formatedImage =>{
                doc.addImage(formatedImage, 15, yLine, 90, 90);
                console.log("Add image: Frequency plot " + plots['frequency'][0].length)
            })
            await loadImage(plots['parallel']).then(formatedImage =>{
                doc.addImage(formatedImage, 115, yLine, 90, 70);
                console.log("Add image: Parallel Coordinates plot " + plots['parallel'][0].length)
            })
        }
        else
        {
            var maxPlotting = plots['parallel'].length;
            var newPage = true;
            console.log('Amount of Parallel Coordinates Plots: ' + plots['parallel'].length)
            console.log('Amount of Frequency Plots: ' + plots['frequency'].length)
            if(plots['frequency'].length > plots['parallel'].length)
                maxPlotting = plots['frequency'].length
            for(i = 0; i < maxPlotting; i++)
            {
                if(maxPlotting <= plots['frequency'].length)
                    await loadImage(plots['frequency'][i]).then(formatedImage =>{
                        doc.addImage(formatedImage, 15, yLine, 95, 95);
                        console.log("Add image: Frequency plot " + (i + 1))
                    });
                if(maxPlotting <= plots['parallel'].length)
                {
                    yLineParallel = yLine;
                    if(i != 0 && !newPage)
                        yLineParallel -= 20
                    newPage = false;
                    await loadImage(plots['parallel'][i]).then(formatedImage =>{
                        doc.addImage(formatedImage, 115, yLineParallel, 90, 70);
                        console.log("Add image: Parallel Coordinates plot " + (i + 1))
                    });
                }
                yLine += (imgHeight + 1);
                if(pageHeight <=  (yLine + imgHeight))
                {
                    doc.setFont("times", "bold");
                    doc.setFontSize(16);
                    doc.text(72, 20, 'IRACE Autogenerated report');
                    doc.setFont("times", "normal");
                    doc.setFontSize(9);
                    yLine = 30;
                    newPage = true;
                    pageHeight = 265;
                    doc.addPage();
                    doc.setPage(doc.internal.getNumberOfPages());
                }
            }
        }
    })
}

async function appendPerfomance(doc, iterations, textIteration, flagConvergence)
{
    var flagLastIteration = false;

    doc.addPage();
    doc.setPage(doc.internal.getNumberOfPages());
    doc.setFont("times", "bold");
    doc.setFontSize(16);
    doc.text(72, 20, 'IRACE Autogenerated report');

    doc.setFont("times", "bold");
    doc.setFontSize(14);
    doc.text(20, 35, 'Perfomance')
    doc.line(19, 36, 48, 36);
    doc.setFontSize(11);
    doc.text(20, 40, textIteration)
    doc.setFont("times", "bold");
    doc.setFontSize(9);
    if(flagConvergence == true)
    {
        doc.text(50, 50, 'Convergence Plot')
        doc.text(46, 53, '(Valid for all iterations)')
        source = $('#convergencePerfomance').children('img')[0].src;

        await loadImage(source).then(formatedImage =>{
            doc.addImage(formatedImage, 20, 54, 80, 80);
            console.log('Add image: Convergence plot')
        });
    }
    else
    {
        if((iterations - 1) != 0)
        {
            var text = 'BoxPlot iteration ' + (iterations - 1)
            doc.text(55, 50, text)
            Shiny.onInputChange("requestPlottingPerfomance", iterations - 1);
            await waitForData(2).then(async plot =>{
                await loadImage(plot['boxPlot']).then(formatedImage =>{
                    doc.addImage(formatedImage, 5, 53, 100, 130);
                    console.log('Add image: Boxplot Perfomance')
                });
            });
        }
        else
            flagLastIteration = true;
    }
    if(flagLastIteration)
        xLine = 80
    else
        xLine = 150

    var text = 'BoxPlot iteration ' + iterations;
    doc.text(xLine, 50, text)

    Shiny.onInputChange("requestPlottingPerfomance", iterations);
    await waitForData(2).then(async plot =>{
        await loadImage(plot['boxPlot']).then(formatedImage =>{
            doc.addImage(formatedImage, (xLine - 45), 53, 100, 130);
            console.log('Add image: Boxplot Perfomance')
        });
    });
}

async function appendDetailsSectionPDF(doc, params)
{
    doc.addPage();
    doc.setPage(doc.internal.getNumberOfPages());
    doc.setFont("times", "bold");
    doc.setFontSize(16);
    doc.text(72, 20, 'IRACE Autogenerated report');

    doc.setFont("times", "bold");
    doc.setFontSize(14);
    doc.text(20, 35, 'Details by iteration')
    doc.line(19, 36, 63, 36);
    doc.setFontSize(11);
    doc.text(20, 40, 'Best-so-far of each iteration')
    doc.setFont("times", "bold");
    doc.setFontSize(9);
    doc.setFont("times", "normal");

    Shiny.onInputChange("requestBestSoFarIterations", params);
    await waitForData(3).then(async valuesTable =>{
        var yLine = 50;
        var pageHeight = 265;
        for(i = 1; i < valuesTable.length; i++)
        {
            doc.setFontSize(9);
            doc.setFont("times", "bold");
            doc.text(20, yLine, ('Iteration ' + i))
            yLine += 5
            doc.setFont("times", "normal");
            doc.text(20, yLine, ('Best-so-far configuration: ' + valuesTable[i]['id']))
            yLine += 4
            doc.text(20, yLine, 'mean value: ' + valuesTable[i]['mean'])
            yLine += 6
            doc.setFontSize(6);
            var width = doc.getTextWidth('ID: ');
            doc.setFont("times", "bold");
            doc.text(20, yLine, 'ID: ')
            doc.setFont("times", "normal");
            doc.text(20 + width + 2, yLine, '' + valuesTable[i]['paramData']['.ID.'][0])
            
            var width = doc.getTextWidth('PARENT: ');
            doc.setFont("times", "bold");
            doc.text(85, yLine, 'PARENT: ')
            doc.setFont("times", "normal");

            if(valuesTable[i]['paramData']['.PARENT.'][0] == null)
                doc.text(85 + width + 2, yLine, 'NA')
            else
                doc.text(85 + width + 2, yLine, '' + valuesTable[i]['paramData']['.PARENT.'][0])

            yLine += 3
            for(var j = 0; j < valuesTable[0].length; j++)
            {
                var width = doc.getTextWidth(valuesTable[0][j]);
                doc.setFont("times", "bold");
                doc.text(20, yLine, valuesTable[0][j] + ':')
                doc.setFont("times", "normal");
                if(valuesTable[i]['paramData'][valuesTable[0][j]] == null)
                    doc.text(20 + width + 2, yLine, 'NA')
                else
                    doc.text(20 + width + 2, yLine, '' + valuesTable[i]['paramData'][valuesTable[0][j]][0])

                if(valuesTable[0][j + 1] != undefined)
                {
                    var width = doc.getTextWidth(valuesTable[0][j + 1]);
                    doc.setFont("times", "bold");
                    doc.text(85, yLine, valuesTable[0][j + 1] + ': ')
                    doc.setFont("times", "normal");

                    if(valuesTable[i]['paramData'][valuesTable[0][j + 1]] == null)
                        doc.text(85 + width + 2, yLine, 'NA')
                    else
                        doc.text(85 + width + 2, yLine, "" + valuesTable[i]['paramData'][valuesTable[0][j + 1]][0])
                    j++;
                }

                if(valuesTable[0][j + 1] != undefined)
                {
                    var width = doc.getTextWidth(valuesTable[0][j + 1]);
                    doc.setFont("times", "bold");
                    doc.text(150, yLine, valuesTable[0][j + 1] + ': ')
                    doc.setFont("times", "normal");

                    if(valuesTable[i]['paramData'][valuesTable[0][j + 1]] == null)
                        doc.text(150 + width + 2, yLine, 'NA')
                    else
                        doc.text(150 + width + 2, yLine, "" + valuesTable[i]['paramData'][valuesTable[0][j + 1]][0])
                    j++;
                }

                yLine += 3
                if(pageHeight <  (yLine + 17))
                {
                    doc.setFont("times", "bold");
                    doc.setFontSize(16);
                    doc.text(72, 20, 'IRACE Autogenerated report');
                    doc.setFont("times", "normal");
                    doc.setFontSize(6);
                    yLine = 35;
                    pageHeight = 265;
                    doc.addPage();
                    doc.setPage(doc.internal.getNumberOfPages());
                }
            }
            yLine += 5;
        }
    });
}

async function appendCustomSectionsPDF(doc)
{
    var sectionsNames = document.getElementById('bestConfigurationSectionSelect').options;
    for(i = 1; i < sectionsNames.length; i++)
    {
        doc.addPage();
        doc.setPage(doc.internal.getNumberOfPages());
        doc.setFont("times", "bold");
        doc.setFontSize(16);
        doc.text(72, 20, 'IRACE Autogenerated report');

        var name = sectionsNames[i].innerHTML;
        var width = doc.getTextWidth(name);
        doc.setFont("times", "bold");
        doc.setFontSize(14);
        doc.text(20, 35, name)
        doc.line(19, 36, width + 23, 36);

        var htmlString = $("#" + sectionsNames[i].value + "Text").summernote("code");
        
        await loadHTMLImage(htmlString, doc).then(customSection =>{
            console.log('Add custom section: ' + name)
        });
    }
}

/* UTILITY FUNCTIONS */

function formatBestConfiguration()
{
    var data = document.getElementById('bestConfigurationsDetails').innerHTML.replace(/♛|➔/g, "").replace(/<[^>]*>/g, "\n").split('\n');
    var formatedData = [];
    var index = 0;

    for(i = 0; i < data.length; i++)
        if(data[i] != "")
        {
            formatedData[index] = data[i].replace(/\s+/g, '');  
            index++;
        }
    return formatedData;
}

function loadImage(src)
{
    return new Promise((resolve, reject) => {
        console.log('Rendering in 1080p resolution...')
        let img = new Image(),
            canvas = document.createElement("canvas"),
            ctx = canvas.getContext("2d");
            // upscale the canvas content
            canvas.width = 1920 * devicePixelRatio;
            canvas.height = 1080 * devicePixelRatio;
            // downscale the presentation
            canvas.style.width = (canvas.width / devicePixelRatio).toString() + "px";
            canvas.style.height = (canvas.height / devicePixelRatio).toString() + "px";
        img.onload = () => {
            ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
            var imgData = canvas.toDataURL('image/jpeg');
            resolve(imgData)
        }
        img.onerror = reject
        img.src = src
    })
}

function loadHTMLImage(htmlString, doc)
{
    var htmlObject = document.createElement("div");
    $(htmlObject).append(htmlString);
    
    var iframe = document.createElement('iframe');
    $('body').append($(iframe));

    var iframedoc = iframe.contentDocument || iframe.contentWindow.document;
    $('body',$(iframedoc)).html(htmlString);
    $("iframe").width(1200);
    return new Promise((resolve, reject) => {
        html2canvas(iframedoc.body, {
            onrendered: function(canvas) {
                var imgData = canvas.toDataURL(
                    'image/png');  

                for (var i = 0; i <= Math.ceil(canvas.height/1650); i++) {

                    var control = 150;

                    if(i == 0)
                    {
                        var sY = (1650 - control)*i;
                        var sHeight = 1650 - control;
                        var dHeight = 1650 - control;
                        control = 1650 - control;
                    }
                    else
                    {
                        var sY = (1650 - control)*i;
                        var sHeight = 1650;
                        var dHeight = 1650;
                        control = 1650;
                    }

                    var srcImg  = canvas;
                    var sX      = 0;
                    var sWidth  = 1200;
                    var dX      = 0;
                    var dY      = 0;
                    var dWidth  = 1200;
            
                    window.onePageCanvas = document.createElement("canvas");
                    onePageCanvas.setAttribute('width', 1200);
                    onePageCanvas.setAttribute('height', control);
                    var ctx = onePageCanvas.getContext('2d');
                    ctx.drawImage(srcImg,sX,sY,sWidth,sHeight,dX,dY,dWidth,dHeight);
            
                    var canvasDataURL = onePageCanvas.toDataURL("image/png", 1.0);
            
                    var width         = onePageCanvas.width;
                    var height        = onePageCanvas.clientHeight;
                    var yLine = 40;
            
                    if (i > 0) {
                        doc.addPage();
                        yLine = 10;
                    }
                    doc.setPage(doc.internal.getNumberOfPages());
                    doc.addImage(canvasDataURL, 'PNG', 15, yLine, (width*.15), (height*.15));
            
                }

                resolve(imgData);
            }
        });
    });
}

/* COMUNICATION WITH BACK-END R */

function waitForData(required)
{
    if(required == 1)
        return new Promise((resolve, reject) => {
            Shiny.addCustomMessageHandler('imagePlotCandidates', 
                function(params) 
                {
                    resolve(params)
                });
        });
    if(required == 2)
        return new Promise((resolve, reject) => {
            Shiny.addCustomMessageHandler('imagePlotPerfomance', 
                function(params) 
                {
                    resolve(params)
                });
        });
    if(required == 3)
        return new Promise((resolve, reject) => {
            Shiny.addCustomMessageHandler('bestSoFarAllIterations', 
                function(params) 
                {
                    resolve(params)
                });
        });
}
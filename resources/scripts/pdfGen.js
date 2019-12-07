async function generatePDF()
{
    var doc = new jsPDF("portrait", "mm", "letter");
    var date = new Date();

    // HEADER
    doc.setFont("times", "bold");
    doc.setFontSize(16);
    doc.text(72, 20, 'IRACE Autogenerated report');

    doc.setFont("times", "normal");
    doc.setFontSize(11);
    doc.text(20, 40, 'Date of the report: ' + date);

    // SUMMARY
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

    // BEST CONFIGURATION
    doc.setFont("times", "bold");
    doc.setFontSize(14);
    doc.text(20, 130, 'Best Configuration')
    doc.line(19, 131, 63, 131);
    doc.setFont("times", "normal");
    doc.setFontSize(9);
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
            doc.setFont("times", "normal");
            doc.setFontSize(9);
            yLine = 30;
            pageHeight = 265;
            doc.addPage();
            doc.setPage(doc.internal.getNumberOfPages());
        }
    }

    // CANDIDATES
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
    doc.text(20, 40, 'Last two iterations')
    doc.setFont("times", "bold");
    doc.setFontSize(9);
    doc.text(50, 50, 'Frequency Plots')
    doc.text(142, 50, 'Parallel Coordinates Plots')

    var iterations = [8, 9];
    Shiny.onInputChange("requestPlottingCandidates", iterations);
    await waitForPlotR(1).then(async plots =>{
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

    // PERFOMANCE
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
    doc.text(20, 40, 'Last iteration')
    doc.setFont("times", "bold");
    doc.setFontSize(9);
    doc.text(50, 50, 'Convergence Plot')
    doc.text(46, 53, '(Valid for all iterations)')
    doc.text(142, 50, 'BoxPlot elite configurations')

    source = $('#convergencePerfomance').children('img')[0].src;

    await loadImage(source).then(formatedImage =>{
        doc.addImage(formatedImage, 20, 54, 80, 80);
        console.log('Add image: Convergence plot')
    });

    var iteration = 9;
    Shiny.onInputChange("requestPlottingPerfomance", iteration);
    await waitForPlotR(2).then(async plot =>{
        await loadImage(plot['boxPlot']).then(formatedImage =>{
            doc.addImage(formatedImage, 100, 53, 100, 130);
            console.log('Add image: Boxplot Perfomance')
        });
    })

    doc.setProperties({
        title: 'IRACE Report',	
        author: 'IRACE-GUI',
        creator: 'IRACE-GUI'
    });

    console.log('Save PDF');
    doc.save('IRACE' + date.getTime() + '.pdf');
}

function formatBestConfiguration()
{
    var data = document.getElementById('bestConfigurationsDetails').innerHTML.replace(/<[^>]*>/g, "\n").replace(/♛|➔/g, "").split('\n');
    var formatedData = [];
    var index = 0;
    for(i = 0; i < data.length; i++)
        if(data[i] != "")
        {
            formatedData[index] = data[i];
            index++
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

function waitForPlotR(required)
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
}
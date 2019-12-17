Shiny.addCustomMessageHandler('loadCustomSections', 
    function(params) 
    {
        appendCustomSections(params);
    });

async function getAllCustomSections()
{
    var newReportName = await inputText("Please, enter the name for the report:", "Name of the report");
    if (newReportName != null && newReportName != 'A name is required')
    {
        var sectionsNames = document.getElementById('bestConfigurationSectionSelect').options;
        var arrayOfSections = [];
        var arrayNameSections = [];
        var arrayIDSections = [];

        for(i = 1; i < sectionsNames.length; i++)
        {
            arrayNameSections[i-1] = sectionsNames[i].innerHTML;
            arrayIDSections[i-1] = sectionsNames[i].value;
            arrayOfSections[i-1] = $("#" + sectionsNames[i].value + "Text").summernote("code");
        }

        Shiny.onInputChange("customSections", arrayOfSections);
        Shiny.onInputChange("customSectionsNames", arrayNameSections);
        Shiny.onInputChange("customSectionsIDS", arrayIDSections);
        Shiny.onInputChange("reportName", newReportName);

        confirmMessage("Report saved in reports folder inside the app folder")
    }
}

function appendCustomSections(customSections)
{
    if(customSections['names'][0].length == 1)
        appendSections(customSections['names'], customSections['ids'], customSections['content'])
    else
        for(i = 0; i < customSections['names'].length; i++)
            appendSections(customSections['names'][i], customSections['ids'][i], customSections['content'][i])
    confirmMessage("Report loaded successfully");
}

function appendSections(name, id, data)
{
    var selectSections = document.getElementsByClassName("section");
    for(k = 0; k < selectSections.length; k++)
    {
        var objOption = document.createElement("option");
        objOption.text = name;
        objOption.value = id;
        objOption.className = id;
        selectSections[k].appendChild(objOption);
    }

    var newSection = document.createElement('a');
    newSection.href = "#" + id + 'Frame';
    newSection.id = id;
    newSection.className = "badge badge-light " + id;
    newSection.style = "color: green; font-size: 18px;";

    var div = document.getElementById('sectionsMenu');
    var idNewSection = '#' + id;
    div.appendChild(newSection);
    $(idNewSection).text('*' + name);

    insertHTMLSection(id, name);
    $('#' + id + 'Text').summernote('code', data);
}
Shiny.addCustomMessageHandler('loadCustomSections', 
    function(params) 
    { 
        appendCustomSections(params)
    });

function getAllCustomSections()
{
    var newReportName = prompt("Please, enter the name for the report:", "New Report");
    if (newReportName == null || newReportName == "")
        alert("User cancelled the prompt.");
    else
    {
        var sectionsNames = document.getElementById('bestConfigurationSectionSelect').options;
        var arrayOfSections = [];
        var arrayNameSections = [];
        var arrayIDSections = [];

        for(i = 1; i < sectionsNames.length; i++)
        {
            arrayNameSections[i-1] = sectionsNames[i].innerHTML;
            arrayIDSections[i-1] = sectionsNames[i].value;

            var plainHTML = $($("#" + sectionsNames[i].value + "Text").summernote("code"))
            arrayOfSections[i-1] = plainHTML[0].outerHTML
        }

        Shiny.onInputChange("customSections", arrayOfSections);
        Shiny.onInputChange("customSectionsNames", arrayNameSections);
        Shiny.onInputChange("customSectionsIDS", arrayIDSections);
        Shiny.onInputChange("reportName", newReportName);
        alert("Report saved in reports folder inside the app folder")
    }
}

function appendCustomSections(customSections)
{
    for(i = 0; i < customSections['names'].length; i++)
    {
        var selectSections = document.getElementsByClassName("section");
        for(k = 0; k < selectSections.length; k++)
        {
            var objOption = document.createElement("option");
            objOption.text = customSections['names'][i];
            objOption.value = customSections['ids'][i];
            objOption.className = customSections['ids'][i];
            selectSections[k].appendChild(objOption);
        }
        insertHTMLSection(customSections['ids'][i], customSections['names'][i]);
        $('#' + customSections['ids'][i] + 'Text').summernote('pasteHTML', customSections['content'][i]);
    }
}
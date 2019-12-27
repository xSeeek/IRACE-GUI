window.onload = function(){
    updateInput();
    getParameters();
    setTimeout(function(){
        checkForChanges();
    }, 6000);
}

function getParameters()
{
    var options = [];
    var index = 0;
    $("#selectedParametersCandidates option").each(function()
    {
        options[index] = {text:$(this).html(), value:$(this).val()}
        index++;
    });
    var $select = $('#selectParametersPDF').selectize({options:options});
    
    var selectize = $select[0].selectize; // Get selectize instance
    selectize.setValue(Object.keys(selectize.options)); // Set all selectize options using setValue() method
}

function updateInput()
{
    var select = document.getElementById('iterationDetails')
    select.setAttribute('onchange','setDatatables();');
}

function setDatatables()
{
   checkForChanges();
}

async function createNewSection()
{
    var newSectionName = await inputText("Please, enter the name for the new section:", "Name of the section");
    if (newSectionName != null && newSectionName != 'A name is required')
    {
        var randomNum = Math.floor(Math.random() * 101);
        var sectionName = newSectionName.replace(/\s/g, "") + randomNum;

        var newSectionContainer = document.createElement('li');
        newSectionContainer.className = "nav-item " + sectionName;
        newSectionContainer.id = sectionName + 'List';

        var newSection = document.createElement('a');
        newSection.href = "#" + sectionName + 'Frame';
        newSection.id = sectionName;
        newSection.className = "nav-link " + sectionName;
        newSection.text = newSectionName;
        newSectionContainer.appendChild(newSection);

        var div = document.getElementById('buttonCustomSections');
        $(newSectionContainer.outerHTML).insertBefore(div);

        insertHTMLSection(newSectionName.replace(/\s/g, "") + randomNum, newSectionName)
        addSectionsToSelect(newSectionName, (newSectionName.replace(/\s/g, "") + randomNum))
    }
}
function insertHTMLSection(id, name)
{
    var textData = 
    '<div class="container ' + id + ' customSection">' + 
        '<div class="row justify-content-center">' +
            '<div class="col-md-12 overflow-auto">' +
                '<div class="card">' +
                        '<div class="card-header" id="' + id + 'Frame">' + name + 
                        '<button type="button" style="float: right" id="' + id + 'Button" class="btn btn-outline-danger"> Delete ' +
                            '<img src="../resources/icons/trash-solid.svg" width="17" height="17" alt="Delete section" align="center">' + 
                        '</button>' +
                        '</div>' + 
                        '<div class="card=body">' + 
                            '<br>' +
                            '<div class="container">' +
                                '<div class="row">' +
                                    '<div class="col">' + 
                                        '<div id="' + id + 'Text" class="TextAreaCustom"></div>' +
                                    '</div>' +
                                '</div>' +
                            '</div>' +
                            '<br>' +
                        '</div>' +
                    '</div>' + 
                '</div>' +
            '</div>' +
        '</div>' +
    '<br class="' + id + ' customSection">'
    var div = document.getElementsByClassName("content-wrapper")[0];
    div.insertAdjacentHTML('beforeend', textData);

    var buttonUpdate = document.getElementById(id + "Button");
    buttonUpdate.onclick = function(){removeSection(id)};

    $('#' + id + 'Text').summernote({
        height: 750,
        minHeight: null,
        maxHeight: null,
        //focus: true,
        spellCheck: true,
        toolbar: [
            ['style', ['style']],
            ['font', ['bold', 'underline', 'clear']],
            ['fontname', ['fontname']],
            ['height', ['height']],
            ['color', ['color']],
            ['para', ['ul', 'ol', 'paragraph']],
            ['table', ['table']],
            ['insert', ['link', 'picture', 'video']],
            ['view', ['fullscreen', 'codeview', 'help']],
        ],
        lineHeights: ['0.2', '0.3', '0.4', '0.5', '0.6', '0.8', '1.0', '1.2', '1.4', '1.5', '2.0', '3.0']
    });
}

function addSectionsToSelect(sectionName, sectionID)
{
    var selectSections = document.getElementsByClassName("section");

    for(i = 0; i < selectSections.length; i++)
    {
        var objOption = document.createElement("option");
        objOption.text = sectionName;
        objOption.value = sectionID;
        objOption.className = sectionID;
        selectSections[i].appendChild(objOption);
    }
}

function copyContentIntoSection(selectID, idDataToCopy, isImg)
{
    var selectData = document.getElementById(selectID).value;
    let clone = document.querySelector('#' + idDataToCopy).cloneNode( true );
    clone.setAttribute('id', (idDataToCopy +  selectID));

    $('#' + selectData + 'Text').summernote('pasteHTML', clone.getElementsByTagName('img'));
}

async function copyTableIntoSection(selectID, tableID, bestTable)
{
    var selectData = document.getElementById(selectID).value;
    var oTable = document.getElementById(tableID);
    var rowLength = oTable.rows.length;
    var formatedHTML = "";
   
    for (var i = 1; i < rowLength; i++){

        var header = oTable.rows.item(0).cells;
        var oCells = oTable.rows.item(i).cells;
        var cellLength = oCells.length;

        if(bestTable)
        {
            formatedHTML += await '<div><b>' + (header.item(0).textContent + ": </b>" + oCells.item(0).textContent) + "&emsp;";
            formatedHTML += await '<b>' + (header.item(cellLength - 1).textContent + ": </b>" + oCells.item(cellLength - 1).textContent) + "<br>";
            cellLength -= 1;
        }
        else
            formatedHTML += await '<div><b>' + (header.item(0).textContent + ": </b>" + oCells.item(0).textContent) + "<br>";

        for(var j = 1; j < cellLength; j++){
            var string = "";

            if(header.item(j) != null && j != cellLength)
            {
                string += await '<b>' + (header.item(j).textContent + ": </b>" + oCells.item(j).textContent) + "&emsp;";
                j++;
            }
            if(header.item(j) != null && j != cellLength)
            {
                string += await '<b>' + (header.item(j).textContent + ": </b>" + oCells.item(j).textContent) + "&emsp;";
                j++;
            }
            if(header.item(j) != null && j != cellLength)
            {
                string += await '<b>' + (header.item(j).textContent + ": </b>" + oCells.item(j).textContent) + "&emsp;";
                j++;
            }
            if(header.item(j) != null && j != cellLength)
                string += await '<b>' + (header.item(j).textContent + ": </b>" + oCells.item(j).textContent) + "<br>";

            formatedHTML += string;
        }
        formatedHTML += '</div>'
    }

    await $('#' + selectData + 'Text').summernote('pasteHTML', (formatedHTML));
}

async function removeSection(sectionID)
{
    if(await confirmDelete("Delete Section", "This action can't be undone", "Delete", "Custom section deleted") == true)
        $('.' + sectionID).remove();
}

function checkForChanges()
{
    if((!$.fn.DataTable.isDataTable( '#bestSoFarSelected' )) && !$('#bestSoFarSelected').hasClass('recalculating'))
    {
        setTimeout(function(){
            $('table.display').DataTable({
                "scrollX": true,
                "scrollY": true,
                columnDefs: [
                    { width: 140, targets: 0 }
                ],
                fixedColumns: true
            });
        }, 250);
    }
    else
        setTimeout(checkForChanges, 250);
}

function backToMainMenu()
{
    Shiny.onInputChange("backMainMenu", true);
}

function loadSetup()
{
    Shiny.onInputChange("launchSetup", Math.random());
}

Shiny.addCustomMessageHandler('closeWindow', 
    function(m) 
    {
        window.close();
    });

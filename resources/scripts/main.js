window.onload = function(){
    setTimeout(function(){
        $('table.display').DataTable({
            "scrollX": true,
            "scrollY": true
        });
    }, 1000);
    updateInput();
}

function updateInput()
{
    var select = document.getElementById('iterationDetails')
    select.setAttribute('onchange','setDatatables();');
}

function setDatatables(){
    setTimeout(function(){
        $('table.display').DataTable({
            "scrollX": true,
            "scrollY": true
        });
    }, 100);
}
function createNewSection()
{
    var newSectionName = prompt("Please, enter the name for the new section:", "New Section");
    if (newSectionName == null || newSectionName == "")
        alert("User cancelled the prompt.");
    else
    {
        var newSection = document.createElement('a');
        var randomNum = Math.floor(Math.random() * 101);
        newSection.href = "#" + newSectionName.replace(/\s/g, "") + randomNum + 'Frame';
        newSection.id = newSectionName.replace(/\s/g, "") + randomNum;
        newSection.className = "badge badge-light " + newSection.id;
        newSection.style = "color: green; font-size: 18px;";

        var div = document.getElementById('sectionsMenu');
        var idNewSection = '#' + (newSectionName.replace(/\s/g, "")) + randomNum;
        div.appendChild(newSection);
        $(idNewSection).text('*' + newSectionName);
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
                                        '<div id="' + id + 'Text"></div>' +
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
    var div = document.getElementsByTagName("BODY")[0];
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

function copyTableIntoSection(selectID, idDataToCopy)
{
    var selectData = document.getElementById(selectID).value;

    let clone = $('#' + (idDataToCopy)).slice();
    clone.attr("id", (idDataToCopy +  selectID));
    clone.removeClass('display');
    clone.DataTable().destroy();

    $('#' + selectData + 'Text').summernote('pasteHTML', clone);
}

function removeSection(sectionID)
{
    $('.' + sectionID).remove();
}
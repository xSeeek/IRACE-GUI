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
        newSection.className = "badge badge-light";
        newSection.id = newSectionName.replace(/\s/g, "") + randomNum;
        newSection.style = "color: lightblue; font-size: 18px;";

        var div = document.getElementById('sectionsMenu');
        var idNewSection = '#' + (newSectionName.replace(/\s/g, "")) + randomNum;
        div.appendChild(newSection);
        $(idNewSection).text('*' + newSectionName);
        insertHTMLSection(newSectionName.replace(/\s/g, "") + randomNum, newSectionName)
    }
}
function insertHTMLSection(id, name)
{
    var textData = 
    '<div class="container">' + 
        '<div class="row justify-content-center">' +
            '<div class="col-md-12 overflow-auto">' +
                '<div class="card">' +
                        '<div class="card-header" id="' + id + 'Frame">' + name + '</div>' + 
                        '<div class="card=body">' + 
                            '<br>' +
                            '<div class="container">' +
                                '<div class="row">' +
                                    '<div class="col">' + 
                                        '<textarea>Write yours annotations here</textarea>' +
                                    '</div>' +
                                    '<div class="col">' + 
                                        'Images and plots:' +
                                        '<br>' +
                                    '</div>' +
                                '</div>' +
                            '</div>' +
                            '<br>' +
                        '</div>' +
                    '</div>' + 
                '</div>' +
            '</div>' +
        '</div>' +
    '<br>'
    var div = document.getElementsByTagName("BODY")[0];
    div.insertAdjacentHTML('beforeend', textData);
}
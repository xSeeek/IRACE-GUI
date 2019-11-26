function getAllCustomSections()
{
    var customSections = document.getElementsByClassName("customSection");
    var arrayOfSections = [];

    for(i = 0; i < customSections.length; i++)
        arrayOfSections[i] = customSections[i].outerHTML

    Shiny.onInputChange("customSections", arrayOfSections);
}
function generatePDF()
{
    var doc = new jsPDF();
    doc.text(20, 20, 'Hello world.');
    doc.save('Test.pdf');
}
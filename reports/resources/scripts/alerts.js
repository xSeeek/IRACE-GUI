function confirmMessage(message)
{
    Swal.fire({
        position: 'top-end',
        icon: 'success',
        title: message,
        showConfirmButton: false,
        timer: 1500
    })
}

function errorMessage(message)
{
    Swal.fire({
        position: 'center',
        icon: 'error',
        title: message,
        showConfirmButton: true
    })
}

async function inputText(message, placeholder)
{
    const { value: input } = await Swal.fire({
        title: message,
        input: 'text',
        showCancelButton: true,
        inputPlaceholder: placeholder,
        inputValidator: (value) => 
        {
            if (!value)
                return 'A name is required'
        }
    });
    if(input)
        return input;
    return null;
}

async function confirmDelete(title, message, titleConfirmation, messageConfiguration)
{
    var status = await Swal.fire({
        title: title,
        text: message,
        icon: 'warning',
        showCancelButton: true,
        confirmButtonColor: '#3085d6',
        cancelButtonColor: '#d33',
        confirmButtonText: 'Delete'
    }).then((result) => {
        if (result.value) {
            Swal.fire(
                titleConfirmation,
                messageConfiguration,
                'success'
            )
            return true;
        }
        return false;
    });
    return status;
}

function showLoading()
{
    Swal.fire({
        title: "Generating PDF...",
        text: "Please wait, this may take a longer time depending on the number of parameters",
        icon: 'success',
        showConfirmButton: false,
        allowEscapeKey: false,
        allowOutsideClick: false,
        onOpen: () => {
            swal.showLoading();
        }
    })
}

function updateTextStatus(msg)
{
    Swal.update({text: msg})
}
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
    })
    if(input)
        return input;
    return null;
}

async function confirmDelete(title, message, titleConfirmation, messageConfiguration)
{
    var status = await Swal.fire({
        title: 'Delete Section',
        text: "You won't be able to revert this!",
        icon: 'warning',
        showCancelButton: true,
        confirmButtonColor: '#3085d6',
        cancelButtonColor: '#d33',
        confirmButtonText: 'Delete'
    }).then((result) => {
        if (result.value) {
            Swal.fire(
                'Deleted!',
                'Custom section deleted.',
                'success'
            )
            return true;
        }
        return false;
    });
    return status;
}
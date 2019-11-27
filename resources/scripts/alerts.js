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
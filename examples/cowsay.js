/* Cowsay */
/*
This example demonstrates: 
- Command Line Inputs
- Cow
*/

// Repeat a string n times
const repeat_str = function(str,n) {
    let result = "";
    for(let i = 0; i < n; i++){
        result += str
    };
    return result
};

// Input
const message = input("Message: ");
const msg_len = size(message);

// Build speech bubble
print(" " + repeat_str("_",msg_len + 2));
print("< " + message + " >");
print(" " + repeat_str("-",msg_len + 2));

// Spaces for cow
const cow_spaces = repeat_str(" ", 3 + msg_len);

// Cow body
const cow_body = [
    "\   ^__^",
    " \  (oo)\_______",
    "    (__)\       )\/\",
    "        ||----w |",
    "        ||     ||"
];

// Print cow body
for(i in cow_body){
    print(cow_spaces + cow_body[i])
}

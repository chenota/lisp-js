/* Coin Flip */
/*
This example demonstrates: 
- Use of random numbers
*/
const flip_coin = function(){
    if(random() > 0.5){
        return 'H'
    };
    return 'T'
};
const num_flips = Number(input("Number of Flips: "));
let flip_str = "";
for(let i = 0; i < num_flips; i++){
    flip_str += flip_coin()
};
print(flip_str)
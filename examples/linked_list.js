/* Functional Linked List */
/*
This example demonstrates: 
- Use of lexical scope to simulate private methods in a 'class'
- How lexically scoped functions can be used as data structures (conscells in this case)
- Use of the 'this' keyword to allow an object to refer to itself
*/

// Linked list "class"
const linked_list = function(){
    // "Private" methods
    const cons = function(x,y){
        return (idx) => (idx ? x : y)
    };
    const car = function(cell){
        if(cell === null){
            return undefined
        };
        return cell(true)
    };
    const cdr = function(cell){
        if(cell === null){
            return undefined
        };
        return cell(false)
    };
    // Linked list object
    return {
        // Member variables
        list: null,
        length: 0,
        // Push value to front of list
        push_front: function(value){
            this.list = cons(value,this.list);
            this.length++;
            return value
        },
        // Push value to rear of list
        push_back: function(value){
            const pb_helper = function(cell){
                if(cell === null){
                    return cons(value,null)
                };
                return cons(car(cell),pb_helper(cdr(cell)))
            };
            this.list = pb_helper(this.list);
            this.length++;
            return value
        },
        // Peek front of list
        peek_front: function(){
            return car(this.list)
        },
        // Pop from front of list
        pop_front: function(){
            const val = car(this.list);
            this.list = cdr(this.list);
            this.length--;
            return val
        },
        // Peek back of list
        peek_back: function(){
            const pb_helper = function(cell){
                if(cell === null){
                    return undefined
                } else if(cdr(cell) === null){
                    return car(cell)
                } else {
                    return pb_helper(cdr(cell))
                }
            };
            return pb_helper(this.list)
        },
        // Pop from back of list
        pop_back: function(){
            let val = undefined;
            const pb_helper = function(cell){
                if(cell === null || cdr(cell) === null){
                    return null
                } else if(cdr(cell) === null){
                    val = car(cell);
                    return null
                } else {
                    return cons(car(cell), pb_helper(cdr(cell)))
                }
            };
            this.list = pb_helper(this.list);
            return val
        },
        // Convert list to str
        to_string: function(){
            const ts_helper = function(cell){
                if(cell === null){
                    return ""
                } else if(cdr(cell) === null){
                    return String(car(cell))
                } else {
                    return (String(car(cell)) + " -> " + ts_helper(cdr(cell)))
                }
            };
            return "( " + ts_helper(this.list) + " )"
        },
        // Read linked list from array
        from_arr: function(arr){
            this.list = null;
            this.length = 0;
            for(i in arr){
                this.push_back(arr[i])
            }
        }
    }
};
const myll = linked_list();
myll.from_arr(["Hello",", ","World","!"]);

print("List: " + myll.to_string())
print("Length: " + myll.length)
print("Front: " + myll.peek_front())
print("Back: " + myll.peek_back())

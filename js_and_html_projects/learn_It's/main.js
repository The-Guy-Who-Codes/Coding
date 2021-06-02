var x = true;
    var finishscore = prompt("what is your finishing score?");
    var score = 0;
    
    
    while (x){
        var start = Math.floor(Math.random() * (11 - 1) + 1);
        var times = Math.floor(Math.random() * (11 - 1) + 1);
        
        if (times != 0 || start != 0){
            var input = prompt("what is " + start + " x " + times);
            var ans = times * start;
            
            if (parseInt(input) == ans){
                confirm("well done you got it correct");
                score ++;
            }
            else{
                confirm("oh no you got it wrong");
                score --;
            }
            if (score <= 0){
                score = 0; 
            }
            confirm("your score is: " + score);
            
            if (score == finishscore){
                var ask = confirm("do you want to carry on?");
                if (ask){
                    confirm("ok");
                    finishscore = prompt("what is your finishing score?");
                    score = 0;
                }
                else{
                    document.write("you have quitted the game");
                    break;
                
                
            }    
        }
    }
}
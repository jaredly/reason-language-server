ReactDOMRe.renderToElementWithId(<Component1 message="Hello!" />, "index1");

ReactDOMRe.renderToElementWithId(<Component2 greeting="Hello!" />, "index2");
/* ----- */
      let a = {
        0;
      }


      let indent = (startPos, text, countLeading) => {
        
        let getFullLineFromPos = (pos, s) => {
          /* TODO: Handle no leading/trailing new line */
      
          let left =
            switch (String.rindex_from(s, pos, '\n') + 1) {
            | pos => pos
            | exception Not_found => 0
            };
          let right =
            switch (String.index_from(s, pos, '\n') - 1) {
            | pos => pos
      
            | exception Not_found => String.length(s) - 1
            };
          String.sub(s, left, right - left);
        };
        getFullLineFromPos(startPos, text) |> countLeading(' ');
      };

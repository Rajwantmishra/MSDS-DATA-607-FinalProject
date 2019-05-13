//https://shiny.rstudio.com/articles/communicating-with-js.html
//http://127.0.0.1:3152/shared/shiny.min.js
window.onload = function() {

	const pieces = document.getElementsByTagName("svg");
	for (var i = 0; pieces.length; i++) {
		let _piece = pieces[i];
		_piece.onclick = function(t) {
			if (t.target.getAttribute("data-position") != null){
				valBodyName = t.target.getAttribute(
					"data-position"
				);
				setINputValueOnUI(valBodyName)
			//	document.getElementById("data").innerHTML = t.target.getAttribute(
			//		"data-position"
			//	);
			//	document.getElementById("BodyName").value= valBodyName
				
			//	Shiny.setInputValue("BodyName", valBodyName);
			}
			if (t.target.parentElement.getAttribute("data-position") != null){
				//Set the name of Body parts
				valBodyName = t.target.parentElement.getAttribute("data-position");
				setINputValueOnUI(valBodyName)
				//document.getElementById(
				//	"data"
				//).innerHTML =valBodyName;
				//document.getElementById("BodyName").value= valBodyName
				
				//Shiny.setInputValue("BodyName", valBodyName);
				// This doen't work
				///document.getElementById("BodyName").value= t.target.parentElement.getAttribute("data-position");
			}



		};

		setINputValueOnUI = function(setValue){

			// alert("IN NEW CODE")

			document.getElementById(
				"data"
			).innerHTML =setValue;
			document.getElementById("BodyName").value= setValue
			
			Shiny.setInputValue("BodyName", setValue);

		}
	}
};

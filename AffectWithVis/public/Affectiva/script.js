//python -m SimpleHTTPServer


// SDK Needs to create video and canvas nodes in the DOM in order to function
// Here we are adding those nodes a predefined div.
var divRoot = $("#affdex_elements")[0];
var width = 680;
var height = 480;
var faceMode = affdex.FaceDetectorMode.LARGE_FACES;
//Construct a CameraDetector and specify the image width / height and face detector mode.
var detector = new affdex.CameraDetector(divRoot, width, height, faceMode);

var safeData = null;


//Initialize arrays to hold the JSON data..... honestly idk what I'm doing
var appearance = [];
var emotions = [];
var expressions = [];
var emojis = [];
var times = [];

//var timeInterval = setInterval(myTimer(timestamp.toFixed(), faces), 1000);


//Enable detection of all Expressions, Emotions and Emojis classifiers.
detector.detectAllEmotions();
detector.detectAllExpressions();
detector.detectAllEmojis();
detector.detectAllAppearance();

//Add a callback to notify when the detector is initialized and ready for runing.
detector.addEventListener("onInitializeSuccess", function() {
  log('#logs', "The detector reports initialized");
});

function log(node_name, msg) {
  $(node_name).append("<span>" + msg + "</span><br />")
}

//function executes when Start button is pushed.
function onStart() {
  if (detector && !detector.isRunning) {
    $("#logs").html("");
    detector.start();
  }
  log('#logs', "Clicked the start button");
}

//function executes when the Stop button is pushed.
function onStop() {
  log('#logs', "Clicked the stop button");
  if (detector && detector.isRunning) {
//	prepCSV();
    detector.removeEventListener();
    detector.stop();  
  }
};


//function executes when the Reset button is pushed.
function onReset() {
  log('#logs', "Clicked the reset button");
  if (detector && detector.isRunning) {
    detector.reset();
    $('#results').html("");
  }
};

//function executes when Download button is pressed
function onDownload(){
  log('#logs', "Clicked the download button");
  //if(detector && detector.isRunning){
  prepCSV();
//	detector.download();

  //}

};


//Add a callback to notify when camera access is allowed
detector.addEventListener("onWebcamConnectSuccess", function() {
  log('#logs', "Webcam access allowed");
  console.log("Webcam access allowed");
});

//Add a callback to notify when camera access is denied
detector.addEventListener("onWebcamConnectFailure", function() {
  log('#logs', "webcam denied");
  console.log("Webcam access denied");
});

//Add a callback to notify when detector is stopped
detector.addEventListener("onStopSuccess", function() {
  log('#logs', "The detector reports stopped");
  $("#results").html("");
});

//Add a callback to receive the results from processing an image.
//The faces object contains the list of the faces detected in an image.
//Faces object contains probabilities for all the different expressions, emotions and appearance metrics

//timeInterval = setInterval(myTimer(timestamp.toFixed(), faces), 1000);


detector.addEventListener("onImageResultsSuccess", function(faces, image,
  timestamp) {
  $('#results').html("");
  log('#results', "Timestamp: " + timestamp.toFixed(2));
  log('#results', "Number of faces found: " + faces.length);
  
  
  if (faces.length > 0) {
  	
  	//JSONs of each feature
  	var cApp = JSON.stringify(faces[0].appearance);
  	
  	var cEmotions = JSON.stringify(faces[0].emotions,
      function(key, val) {
        return val.toFixed ? Number(val.toFixed(0)) : val;
      });
      
    var cExpr = JSON.stringify(faces[0].expressions,
      function(key, val) { 
        return val.toFixed ? Number(val.toFixed(0)) : val;
      });
      
    var cEmoji = faces[0].emojis.dominantEmoji;
    
    log('#results', "Emotions: " + cEmotions);


    //Gather data every second
    if ((timestamp.toFixed(1) % 1) == 0){
    	//Add data to arrays
		//appearance.push(cApp);
		emotions.push(faces[0].emotions);
		console.log(faces[0].emotions);
		//expressions.push(cExpr);
		//emojis.push(cEmoji);  
		times.push(timestamp);
     }
 } 
});




//My new code
function prepCSV(){
	
	//{"joy":0,"sadness":0,"disgust":0,"contempt":0,"anger":0,"fear":0,"surprise":0,"valence":0,"engagement":0},
    var headers = "Time, Joy, Sadness, Disgust, Contempt, Anger, Fear, Surprise, Valence, Engagement \r\n";
	var fileTitle = "affecttest"; 
	
	//Format for CSV
	dataFormatted = convertToCSV(times, emotions);
	// call the exportCSVFile() function to process the JSON and trigger the download
	exportCSVFile( headers, dataFormatted, fileTitle); 
}



/* converts objArray of emotion data into CSV format
	Input: times = obj array of times, integers
			emotions = obj array of obj arrays of emotion data, floats
		ie: [[10.2,11.2,12.2],[10.2,11.2,12.2],[10.2,11.2,12.2]]
	Output: Str object, CSV format
*/	
function convertToCSV(times, emotions) {
	//log('#logs', "dataArr:"+ itemsFormatted);
	var array = typeof emotions != 'object' ? JSON.parse(emotions) : emotions;
    var str = '';



	//NOTE: Fix: NOT COLLECTING ENGAGEMENT
    for (var i = 0; i < emotions.length; i++) {
        var line = '';
        for (index in emotions[i]) {
        	//console.log(emotions[i][index]);
            if (line != ''){
            	line += ',';
            	line += emotions[i][index].toFixed(2);
            }
            else{
            	line += times[i].toFixed(0);
            	line += ',';
            	line += emotions[i][index].toFixed(2);
            }
        }
        console.log("line " + i + ": " + line);
        str += line + '\r\n';
    }
    return str;
}



function exportCSVFile(headers, items, fileTitle) {
    if (headers) {
        items = headers + items;     
    }

    var csv = items;

    var exportedFilename = fileTitle + '.csv' || 'export.csv';

    var blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });
    if (navigator.msSaveBlob) { // IE 10+
        navigator.msSaveBlob(blob, exportedFilenmae);
    } else {
        var link = document.createElement("a");
        if (link.download !== undefined) { // feature detection
            // Browsers that support HTML5 download attribute
            var url = URL.createObjectURL(blob);
            link.setAttribute("href", url);
            link.setAttribute("download", exportedFilename);
            link.style.visibility = 'visible';
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
        }
    }
}

//End of my new code



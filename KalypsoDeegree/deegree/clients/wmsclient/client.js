var MAX_LAYERS = 15;
function addLayer() {

	 // register form
	 var form = window.document.mapform;
	
		
	 var src = form.AVAILABLELAYERS;
	 var target = form.LAYERLIST;	
			 
	 var idx = src.selectedIndex;
	 var value = src.options[idx].value + '|default';
	 var name =  src.options[idx].text;
	 
	 // create new item to add to listbox
	 var item = new Option( name, value, false);
	 if (target.length > MAX_LAYERS)
	     alert( "Sie k�n nicht mehr als 15 Ebenen ausw㧬en." );
	 else
		 target.options[target.length] = item;
}

function addNamedLayer(name, value ) {

	 // register form
	 var form = window.document.mapform;
				 
	 var target = form.LAYERLIST;	
			 
	 var item = new Option( name, value, false);
	 item.value = value;	 	 
	 target.options[target.length] = item;

}


function getLayerListAsString() {

	var s = '';

	// register form
	var form = window.document.mapform;
	
	// register listbox
	var box = form.LAYERLIST;
	
	if (box != null) {
		for (var i= 0; i < box.length; i++) {
				s = s + box.options[i].value;
				if ( i < box.length-1 ) {
					s = s + ',';
				}
		}
	} 	
	return s;	 
}


function removeFromList() {

	 // register form
	var form = window.document.mapform;
	
	// register listbox
	var box = form.LAYERLIST;
	
	if (box == null) {
	 alert( "Listenfeld ist nicht korrekt registriert." );
	 return false;
	}
	
	// delete item
	var idx = box.selectedIndex;
	if (idx != null) box.options[idx] = null;
	// select item above
	if ((idx-1) >= 0) box.options[idx-1].selected = true;
 
}

function clearList() {
	// register form
	var form = window.document.mapform;
	
	// register listbox
	var box = form.LAYERLIST;
	
	if (box != null) {
		for (var i= box.length-1; i >= 0; i--) {
			box.options[i] = null;
		}
	}
	
}

function move ( direction ) {
	 // register form
   var form = window.document.mapform;

	 // register listbox
	 var box = form.LAYERLIST;	 
	 if (box == null) {
	 		 alert( "Listenfeld ist nicht korrekt registriert." );
			 return false;
	 }

	 var idx = box.selectedIndex;
	 
   // get value to be moved
	 var value = box.options[idx].value;
	 var name = box.options[idx].text;
     var item = new Option( name, value, true, true);

	 var idx2 = null;
	 if ( direction == 'up' )
	    idx2 = parseInt(idx - 1);
	 else if ( direction == 'down' )
	    idx2 = parseInt(idx + 1);

	 // nothing to do if end or begin of list is reached
	 if ( ( idx2 < 0 ) || ( idx2 >= box.length) )
	    return;
			
	 var value2 = box.options[idx2].value;
	 var name2 = box.options[idx2].text;
	 var item2 = new Option( name2, value2, false);
			
   box.options[idx] = item2;
   box.options[idx2] = item;
}

function selectLayers() {
	 // register form
   var form = window.document.mapform;

	 // register listbox
	 var box = form.LAYERLIST;
	 
	 if (box != null) {
	 		for (var i= 0; i < box.length; i++) {
	 		 		box.options[i].selected = true;
	 		}
	 }
	 else {
	 		return false;
	 }
	 
	 return true;	 
}



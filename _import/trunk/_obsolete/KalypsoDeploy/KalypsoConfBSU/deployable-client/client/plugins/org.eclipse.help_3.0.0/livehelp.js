/*******************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

/**
 * Private helper function for use by other (public) functions. 
 */
function findHelpTop() {
	var helpTop;
	for (helpTop=self; helpTop; helpTop = helpTop.parent){
		if (helpTop.liveActionInternal){
			break;
		}
		if (helpTop==helpTop.parent){
			break;
		}
	}
	return helpTop;
}

/**
 * Call this Javascript method to trigger a specified live help action
 * in the workbench. 
 * The parameters for liveAction  are:
 * - the id of the plug-in that contains the action
 * - the name of the class that implements the action
 * - the String that will be passed to the live help action using setInitializationString
 */

function liveAction(pluginId, className, argument)
{
	// find top help frameset
	var helpTop=findHelpTop();
	if (helpTop != null && helpTop.liveActionInternal){
		return helpTop.liveActionInternal(helpTop, pluginId, className, argument);
	}
}

/**
 * Show specified topic in the Contents tree.
 * The topic must be passed as a URL string.
 * Example:
 *  // include the script first
 *  <script src="../org.eclipse.help/livehelp.js"></script>
 *  ......
 *  // show specified topic in the tree
 *  showTopicInContents(window.location.href); 
 */
function showTopicInContents(topic) {
		var helpTop=findHelpTop();
		if (helpTop != null && helpTop.showTopicInContentsInternal){
			return helpTop.showTopicInContentsInternal(helpTop, topic);
		}
}

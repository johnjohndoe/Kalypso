/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.editor.styleeditor.dialogs;


public class FeatureIDData extends AbstractData
{
	private String featureId = null;
	
	public String getFeatureId() {
		return featureId;
	}
	public void setFeatureId(String featureId) {
		this.featureId = featureId.trim();
	}	
	
	public boolean verify() throws FilterDialogException 
	{
		if(featureId == null || featureId.trim().length() == 0)		
			throw new FilterDialogException(new FilterDialogError(null,FilterDialogError.INCOMPLETE));
		else 
			return true;		
	}	
}
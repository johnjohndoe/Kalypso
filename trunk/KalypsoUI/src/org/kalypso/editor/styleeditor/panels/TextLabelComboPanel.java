/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.editor.styleeditor.panels;

import java.util.ArrayList;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.swt.widgets.*;

/**
 * @author Administrator
 *  
 */
public class TextLabelComboPanel extends ComboPanel {

	public TextLabelComboPanel(Composite parent, String label, FeatureType featureType, String value) 
	{
		super(parent, label);
		// read possible items to get the label text from
    	ArrayList labelStringItems = new ArrayList();
    	FeatureTypeProperty[] ftp = featureType.getProperties();
    	for(int i=0; i<ftp.length; i++)    	
    		if(!ftp[i].getType().startsWith("org.deegree.model.geometry."))
    			labelStringItems.add(ftp[i].getName()); 
    	items = new String[labelStringItems.size()];
    	for(int j=0; j<items.length; j++)
    		items[j] = (String)labelStringItems.get(j);     	
		init();
		comboBox.setText("...");
		if(value != null)
		{			
			for(int m=0; m<items.length; m++)
				if(items[m].equals(value))
				{
					comboBox.select(m);
					break;
				}
		}			
	}
	
	public String getSelectedFeatureTypeProperty()
	{		
		return items[getSelection()]; 
	}

	// sets the comboBox to a default state
	public void reset()
	{
		comboBox.setText("...");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.kalypso.editor.styleeditor.panels.StrokeComboPanel#getSelection()
	 */
	public int getSelection() {
		return selection_index;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.kalypso.editor.styleeditor.panels.StrokeComboPanel#setSelection(int)
	 */
	public void setSelection(int index) {
		selection_index = index;		
	}
}
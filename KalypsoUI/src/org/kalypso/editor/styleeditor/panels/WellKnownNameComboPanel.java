/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.editor.styleeditor.panels;

import org.eclipse.swt.widgets.*;

/**
 * @author Administrator
 *
 */
public class WellKnownNameComboPanel extends ComboPanel{	
	
	public WellKnownNameComboPanel(Composite parent, String label, String value){
		super(parent,label);
		items = new String[6];				
		items[0] = "square";
		items[1] = "circle";		
		items[2] = "triangle";
		items[3] = "star";
		items[4] = "cross";
		items[5] = "x";
		init();
		int i=0;
		for(; i<items.length; i++)
			if(items[i].equals(value))
				break;		
		setSelection(i);
	}

	/* (non-Javadoc)
	 * @see org.kalypso.editor.styleeditor.panels.StrokeComboPanel#getSelection()
	 */
	public int getSelection() 
	{
		return selection_index;
	}
	
	public static String getWellKnownNameByIndex(int index)
	{
		switch (index) {
		case 0:
			return "square";
		case 1:
			return "circle";
		case 2:
			return "triangle";
		case 3:
			return "star";	
		case 4:
			return "cross";	
		case 5:
			return "x";				
		default:
			return "square";
		}
	}

	/* (non-Javadoc)
	 * @see org.kalypso.editor.styleeditor.panels.StrokeComboPanel#setSelection(int)
	 */
	public void setSelection(int index) {
		comboBox.select(index);	
	}			
}
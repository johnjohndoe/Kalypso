/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.ui.editor.styleeditor.panels;

import org.deegree.graphics.sld.LinePlacement;
import org.eclipse.swt.widgets.*;

/**
 * @author Administrator
 *
 */
public class LabelPlacementComboPanel extends ComboPanel{	
		
	public LabelPlacementComboPanel(Composite parent, String label, int value){
		super(parent,label);
		items = new String[4];				
		items[0] = "above";
		items[1] = "below";
		items[2] = "center";
		items[3] = "auto";
		init();				
		setSelection(value);
	}

	/* (non-Javadoc)
	 * @see org.kalypso.ui.editor.styleeditor.panels.StrokeComboPanel#getSelection()
	 */
	public int getSelection() {
		switch (selection_index) {
		case 0:
			return LinePlacement.TYPE_ABOVE;
		case 1:
			return LinePlacement.TYPE_BELOW;
		case 2:
			return LinePlacement.TYPE_CENTER;
		case 3:
			return LinePlacement.TYPE_AUTO;			
		default:
			return LinePlacement.TYPE_AUTO;
		}
	}

	/* (non-Javadoc)
	 * @see org.kalypso.ui.editor.styleeditor.panels.StrokeComboPanel#setSelection(int)
	 */
	public void setSelection(int index) {		
		switch (index) {
		case LinePlacement.TYPE_ABOVE:
			comboBox.select(0); break;
		case LinePlacement.TYPE_BELOW:
			comboBox.select(1); break;
		case LinePlacement.TYPE_CENTER:
			comboBox.select(2); break;
		case LinePlacement.TYPE_AUTO:
			comboBox.select(3); break;		
		default:
			comboBox.select(3);
		}			
	}			
}
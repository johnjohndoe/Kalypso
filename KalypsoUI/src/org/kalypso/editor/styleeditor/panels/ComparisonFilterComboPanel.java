/*
 *
 */
package org.kalypso.editor.styleeditor.panels;

import org.eclipse.swt.widgets.*;

/**
 * @author Administrator
 *
 */
public class ComparisonFilterComboPanel extends FilterComboPanel{	
		
	public ComparisonFilterComboPanel(Composite parent){
		super(parent);
		items = new String[8];
		items[0] = "BETWEEN";
		items[1] = "LIKE";
		items[2] = "NULL";
		items[3] = "EQUAL_TO";
		items[4] = "LESS_THAN";
		items[5] = "GREATER_THAN";
		items[6] = "LESS_THAN_OR_EQUAL_TO";
		items[7] = "GREATER_THAN_OR_EQUAL_TO";
		// not implemented yet
		//items[8] = "NOT_EQUAL_TO";
		init();
	}

	
	public int getSelection() {
		return selection_index;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.editor.styleeditor.panels.StrokeComboPanel#setSelection(int)
	 */
	public void setSelection(int index) {
		selection_index = index;
		comboBox.select(index);
	}			
}
/*
 *
 */
package org.kalypso.ui.editor.styleeditor.panels;

import org.eclipse.swt.widgets.*;

/**
 * @author Administrator
 *
 */
public class LogicalFilterComboPanel extends FilterComboPanel{	
		
	public LogicalFilterComboPanel(Composite parent){
		super(parent);
		items = new String[3];
		items[0] = "AND";
		items[1] = "OR";
		items[2] = "NOT";			
		init();
	}

	
	public int getSelection() {
		return selection_index;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.ui.editor.styleeditor.panels.StrokeComboPanel#setSelection(int)
	 */
	public void setSelection(int index) {
		selection_index = index;
		comboBox.select(index);
	}			
}
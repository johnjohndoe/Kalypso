/*
 *
 */
package org.kalypso.editor.styleeditor.panels;

import org.eclipse.swt.widgets.*;
import org.kalypso.ui.editor.styleeditor.panels.FilterComboPanel;

/**
 * @author Administrator
 *
 */
public class ColorPaletteComboBox extends FilterComboPanel{	
		
	public ColorPaletteComboBox(Composite parent){
		super(parent);
		items = new String[4];
		items[0] = "RedGreen";
		items[1] = "BlueGreen";
		items[2] = "RedBlue";			
		items[3] = "Custom";
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
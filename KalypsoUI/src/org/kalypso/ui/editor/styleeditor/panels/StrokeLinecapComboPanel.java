/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.ui.editor.styleeditor.panels;

import org.deegree.graphics.sld.Stroke;
import org.eclipse.swt.widgets.Composite;

/**
 * @author Administrator
 *
 */
public class StrokeLinecapComboPanel extends ComboPanel{	
		
	public StrokeLinecapComboPanel(Composite parent, String label, int value){
		super(parent,label);
		items = new String[3];
		items[0] = "round";
		items[1] = "square";
		items[2] = "butt";			
		init();
		setSelection(value);
	}

	/* (non-Javadoc)
	 * @see org.kalypso.ui.editor.styleeditor.panels.StrokeComboPanel#getSelection()
	 */
	public int getSelection() {
		switch (selection_index) {
		case 0:
			return Stroke.LC_ROUND;
		case 1:
			return Stroke.LC_SQUARE;
		case 2:
			return Stroke.LC_BUTT;
		default:
			return Stroke.LC_DEFAULT;
		}
	}

	/* (non-Javadoc)
	 * @see org.kalypso.ui.editor.styleeditor.panels.StrokeComboPanel#setSelection(int)
	 */
	public void setSelection(int index) {
		switch (index) {
		case Stroke.LC_ROUND:
			comboBox.select(0); break;
		case Stroke.LC_SQUARE:
			comboBox.select(1); break;
		case Stroke.LC_BUTT:
			comboBox.select(2); break;
		default:
			comboBox.select(2);
		}	
	}			
}
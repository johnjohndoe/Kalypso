/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.ui.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;

/**
 * @author Administrator
 *
 */
public abstract class FilterComboPanel {
	
	private Composite parent = null;
	private Composite composite = null;	
	 
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;		
	protected Combo comboBox = null;
	protected String items[];
	protected int selection_index = -1;
		
	protected FilterComboPanel(Composite parent){		
		this.parent = parent; 		
		composite = new Composite(parent, SWT.NULL);
		FormLayout compositeLayout = new FormLayout();
		GridData compositeData = new GridData();
		compositeData.widthHint = 90;
		composite.setLayoutData(compositeData);			
		composite.setLayout(compositeLayout);						
		compositeLayout.marginWidth = 0;
		compositeLayout.marginHeight = 0;
		compositeLayout.spacing = 0;
		composite.layout();					
	}	
	
	public String getSelectionName(int index)
	{
		if(index<items.length)
			return items[index];
		else
			return null;
	}
	
	public void addPanelListener(PanelListener pl) {
		listenerList.add(PanelListener.class, pl);
	}

	public abstract void setSelection(int index);
	
	public abstract int getSelection();
	
	public void disable()
	{
		comboBox.setEnabled(false);		
	}
	
	public void enable()
	{
		comboBox.setEnabled(true);
	}
	
	protected void init(){				
	
		comboBox = new Combo(composite, SWT.NULL);		
		FormData comboData = new FormData();				
		comboData.width = 60;
		comboData.height = 10;
		comboData.left =  new FormAttachment(0, 1000, 0);
		comboData.top =  new FormAttachment(0, 1000, 0);
		comboBox.setLayoutData(comboData);					
		comboBox.setItems(items);		
		
		comboBox.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				selection_index = ((Combo)e.getSource()).getSelectionIndex();
				fire();				
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});
		if(items.length>0)
			setSelection(0);
	}
	
    protected void fire() {
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length - 2; i >= 0; i -= 2) {
        	if (listeners[i] == PanelListener.class) {
        		PanelEvent event = new PanelEvent(this);                
                ((PanelListener)listeners[i+1]).valueChanged(event);
        	}
        }
    }	
}
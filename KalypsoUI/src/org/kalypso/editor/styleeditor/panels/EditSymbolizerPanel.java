/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.*;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

/**
 * @author Administrator
 *
 */
public class EditSymbolizerPanel {
	
	private Composite parent = null;
	private Composite composite = null;
	 
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;
		
	public final static int REM_SYMB = 0;
	public final static int FOR_SYMB = 1;
	public final static int BAK_SYMB = 2;
	private int currentAction = -1;
	private int canDelete = -1;
	
	
	public EditSymbolizerPanel(Composite parent, int size){
		this.parent = parent; 
		this.canDelete = size;
		composite = new Composite(parent, SWT.NULL);			
		FormLayout compositeLayout = new FormLayout();
		GridData compositeData = new GridData();
		compositeData.widthHint = 230;
		composite.setLayoutData(compositeData);
		composite.setLayout(compositeLayout);	
		compositeLayout.marginWidth = 0;
		compositeLayout.marginHeight = 0;
		compositeLayout.spacing = 0;
		composite.layout();			
		init();
	}
	
	public void addPanelListener(PanelListener pl) {
		listenerList.add(PanelListener.class, pl);
	}

	
	private void init()
	{			
		Button removeSymbolizerButton = new Button(composite,SWT.PUSH);
		if(canDelete == 0)
			removeSymbolizerButton.setEnabled(false);
		FormData removeSymbolizerButtonData = new FormData();
		removeSymbolizerButtonData.height = 18;
		removeSymbolizerButtonData.width = 18;
		removeSymbolizerButtonData.left =  new FormAttachment(250, 1000, 0);		
		removeSymbolizerButtonData.top =  new FormAttachment(100, 1000, 0);
		removeSymbolizerButton.setLayoutData(removeSymbolizerButtonData);		
		removeSymbolizerButton.setText("-");
		removeSymbolizerButton.setToolTipText("Remove Symbolizer");
		removeSymbolizerButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) 
			{
				currentAction = REM_SYMB;
				fire();							
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});							
	
		Button moveBackwardSymbolizerButton = new Button(composite,SWT.PUSH);
		if(canDelete <= 1)
			moveBackwardSymbolizerButton.setEnabled(false);		
		FormData moveBackwardSymbolizerButtonData = new FormData();
		moveBackwardSymbolizerButtonData.height = 18;
		moveBackwardSymbolizerButtonData.width = 18;
		moveBackwardSymbolizerButtonData.left =  new FormAttachment(500, 1000, 0);		
		moveBackwardSymbolizerButtonData.top =  new FormAttachment(100, 1000, 0);
		moveBackwardSymbolizerButton.setLayoutData(moveBackwardSymbolizerButtonData);		
		moveBackwardSymbolizerButton.setText("<-");
		moveBackwardSymbolizerButton.setToolTipText("Move Backward");
		moveBackwardSymbolizerButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) 
			{
				currentAction = BAK_SYMB;
				fire();							
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});	
		
		Button moveForwardSymbolizerButton = new Button(composite,SWT.PUSH);
		if(canDelete <= 1)
			moveForwardSymbolizerButton.setEnabled(false);			
		FormData moveForwardSymbolizerButtonData = new FormData();
		moveForwardSymbolizerButtonData.height = 18;
		moveForwardSymbolizerButtonData.width = 18;
		moveForwardSymbolizerButtonData.left =  new FormAttachment(750, 1000, 0);		
		moveForwardSymbolizerButtonData.top =  new FormAttachment(100, 1000, 0);
		moveForwardSymbolizerButton.setLayoutData(moveForwardSymbolizerButtonData);		
		moveForwardSymbolizerButton.setText("->");
		moveForwardSymbolizerButton.setToolTipText("Move Forward");
		moveForwardSymbolizerButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) 
			{
				currentAction = FOR_SYMB;
				fire();							
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});								
	}	
	
	public int getAction(){
		return currentAction;
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
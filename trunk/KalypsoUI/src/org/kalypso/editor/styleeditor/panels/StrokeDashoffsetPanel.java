/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

/**
 * @author Administrator
 *
 */
public class StrokeDashoffsetPanel {
	
	private Composite parent = null;
	private Composite composite = null;	
	 
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;
	private String label = null;
	private Text offsetInput = null;
	private float value = 0;
		
	public StrokeDashoffsetPanel(Composite parent, String label, float value){
		this.label = label;	
		this.parent = parent; 
		this.value = value;
		composite = new Composite(parent, SWT.NULL);
		FormLayout compositeLayout = new FormLayout();
		GridData compositeData = new GridData();
		compositeData.widthHint = 200;
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

	
	private void init(){
		
		offsetInput = new Text(composite, SWT.BORDER);
		FormData offsetInputData = new FormData();
		offsetInputData.height = 10;
		offsetInputData.width = 20;		
		offsetInputData.left =  new FormAttachment(340, 1000, 0);
		offsetInputData.top =  new FormAttachment(100, 1000, 0);
		offsetInput.setLayoutData(offsetInputData);	
		offsetInput.setText("0.0");	
		
		Button okButton = new Button(composite, SWT.PUSH);
		FormData okButtonData = new FormData();
		okButtonData.height = 18;
		okButtonData.width = 20;		
		okButtonData.left =  new FormAttachment(540, 1000, 0);
		okButtonData.top =  new FormAttachment(100, 1000, 0);
		okButton.setLayoutData(okButtonData);					
		okButton.setText("Ok");
		okButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				Float fl = null;
				try{
					fl = new Float(offsetInput.getText());
					value = fl.floatValue();					
					fire();					
				}
				catch(NumberFormatException nfe){
					//TODO
					IStatus status = new Status(IStatus.ERROR,"org.kalypso.editor.mapeditor.views.styleeditor",0,"InputError-Stroke-Offset", nfe);					
					ErrorDialog.openError(composite.getShell(), "Input-Error","Input needs to be of type float",status);					
					offsetInput.setText("" +value);
				}
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});
	
		Label offsetLabel = new Label(composite,SWT.NULL);					
		FormData offsetLabelData = new FormData();
		offsetLabelData.height = 15;
		offsetLabelData.width = 242;
		offsetLabelData.left =  new FormAttachment(0, 1000, 0);		
		offsetLabelData.top =  new FormAttachment(100, 1000, 0);
		offsetLabel.setLayoutData(offsetLabelData);			
		offsetLabel.setText(label);		
	}
	
	public float getValue(){
		return value;
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
/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.kalypso.editor.styleeditor.dialogs.errordialog.StyleEditorErrorDialog;


/**
 * @author Administrator
 *
 */
public class StrokeDasharrayPanel {
	
	private Composite parent = null;
	private Composite composite = null;	
	 
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;
	private String label = null;
	private Text lineInput = null;
	private Text spaceInput = null;	
	private float lineValue = 0;
	private float spaceValue = 0;
		
	public StrokeDasharrayPanel(Composite parent, String label, float[] values){
		this.label = label;	
		this.parent = parent; 
		if(values != null && values.length>=2)
		{
			lineValue = values[0];
			spaceValue = values[1];
		}
		composite = new Composite(parent, SWT.NULL);
		FormLayout compositeLayout = new FormLayout();
		GridData compositeData = new GridData();
		compositeData.widthHint = 203;
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
	
		Button okButton = new Button(composite, SWT.PUSH);
		FormData okButtonData = new FormData();
		okButtonData.height = 18;
		okButtonData.width = 20;		
		okButtonData.left =  new FormAttachment(910, 1000, 0);
		okButtonData.top =  new FormAttachment(100, 1000, 0);
		okButton.setLayoutData(okButtonData);					
		okButton.setText("Ok");		
		
		spaceInput = new Text(composite, SWT.BORDER);
		FormData offsetInputData = new FormData();
		offsetInputData.height = 10;
		offsetInputData.width = 16;		
		offsetInputData.left =  new FormAttachment(760, 1000, 0);
		offsetInputData.top =  new FormAttachment(100, 1000, 0);
		spaceInput.setLayoutData(offsetInputData);	
		spaceInput.setText(""+spaceValue);			
		
		Label spaceLabel = new Label(composite, SWT.NULL);
		FormData spaceLabelData = new FormData();
		spaceLabelData.height = 16;
		spaceLabelData.width = 5;
		spaceLabelData.left =  new FormAttachment(740, 1000, 0);		
		spaceLabelData.top =  new FormAttachment(100, 1000, 0);
		spaceLabel.setLayoutData(spaceLabelData);	
		spaceLabel.setText("/");		
		
		lineInput = new Text(composite, SWT.BORDER);
		FormData lineInputData = new FormData();
		lineInputData.height = 10;
		lineInputData.width = 16;		
		lineInputData.left =  new FormAttachment(590, 1000, 0);
		lineInputData.top =  new FormAttachment(100, 1000, 0);
		lineInput.setLayoutData(lineInputData);	
		lineInput.setText(""+lineValue);	
		
		final Label lineLabel = new Label(composite, SWT.NULL);
		FormData lineLabelData = new FormData();
		lineLabelData.height = 15;
		lineLabelData.width = 50;
		lineLabelData.left =  new FormAttachment(340, 1000, 0);		
		lineLabelData.top =  new FormAttachment(100, 1000, 0);
		lineLabel.setLayoutData(lineLabelData);	
		lineLabel.setText("line/space");	
	
		Label offsetLabel = new Label(composite,SWT.NULL);					
		FormData offsetLabelData = new FormData();
		offsetLabelData.height = 15;
		offsetLabelData.width = 242;
		offsetLabelData.left =  new FormAttachment(0, 1000, 0);		
		offsetLabelData.top =  new FormAttachment(100, 1000, 0);
		offsetLabel.setLayoutData(offsetLabelData);			
		offsetLabel.setText(label);
		
		okButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				Float lineFloat = null;
				Float spaceFloat = null;
				try{
					lineFloat = new Float(lineInput.getText());					
					spaceFloat = new Float(spaceInput.getText());
					lineValue = lineFloat.floatValue();
					if(lineValue<=0.0)
					{
						lineValue = 1f;
						lineLabel.setText(""+lineValue);
					}
					spaceValue = spaceFloat.floatValue();
					fire();					
				}
				catch(NumberFormatException nfe){
					//TODO
					StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog(composite.getShell(),"Input needs to be of type float","InputError-Stroke-Dasharray");
					errorDialog.showError();														
					lineInput.setText("" +lineFloat);
					spaceInput.setText(""+spaceValue);
				}
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});		
	}
	
	public float[] getValue(){
		float returnArray[] = {lineValue, spaceValue};
		return returnArray;
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
/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.ui.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;


/**
 * @author Administrator
 *
 */
public class LabelPointPlacementPanel {
	
	private Composite parent = null;
	private Composite composite = null;	
	 
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;
	private String label = null;
	private Text xInput = null;
	private Text yInput = null;	
	private double xValue = 0;
	private double yValue = 0;
		
	public LabelPointPlacementPanel(Composite parent, String label, double[] values){
		this.label = label;	
		this.parent = parent; 
		if(values != null && values.length>=2)
		{
			xValue = values[0];
			yValue = values[1];
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
		
		yInput = new Text(composite, SWT.BORDER);
		FormData offsetInputData = new FormData();
		offsetInputData.height = 10;
		offsetInputData.width = 16;		
		offsetInputData.left =  new FormAttachment(760, 1000, 0);
		offsetInputData.top =  new FormAttachment(100, 1000, 0);
		yInput.setLayoutData(offsetInputData);	
		yInput.setText(""+yValue);			
		
		Label yLabel = new Label(composite, SWT.NULL);
		FormData yLabelData = new FormData();
		yLabelData.height = 16;
		yLabelData.width = 5;
		yLabelData.left =  new FormAttachment(740, 1000, 0);		
		yLabelData.top =  new FormAttachment(100, 1000, 0);
		yLabel.setLayoutData(yLabelData);	
		yLabel.setText("/");		
		
		xInput = new Text(composite, SWT.BORDER);
		FormData xInputData = new FormData();
		xInputData.height = 10;
		xInputData.width = 16;		
		xInputData.left =  new FormAttachment(590, 1000, 0);
		xInputData.top =  new FormAttachment(100, 1000, 0);
		xInput.setLayoutData(xInputData);	
		xInput.setText(""+xValue);	
		
		final Label xLabel = new Label(composite, SWT.NULL);
		FormData xLabelData = new FormData();
		xLabelData.height = 15;
		xLabelData.width = 50;
		xLabelData.left =  new FormAttachment(340, 1000, 0);		
		xLabelData.top =  new FormAttachment(100, 1000, 0);
		xLabel.setLayoutData(xLabelData);	
		xLabel.setText("x/y");	
	
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
				Double xDouble = null;
				Double yDouble = null;
				try{
					xDouble = new Double(xInput.getText());					
					yDouble = new Double(yInput.getText());
					xValue = xDouble.doubleValue();
					if(xValue<=0.0)
					{
						xValue = 1f;
						xLabel.setText(""+xValue);
					}
					yValue = yDouble.doubleValue();
					fire();					
				}
				catch(NumberFormatException nfe){
					//TODO
					IStatus status = new Status(IStatus.ERROR,"org.kalypso.ui.editor.mapeditor.views.styleeditor",0,"InputError-Stroke-Dasharray", nfe);					
					ErrorDialog.openError(composite.getShell(), "Input-Error","Input needs to be of type float",status);					
					xInput.setText("" +xDouble);
					yInput.setText(""+yValue);
				}
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});		
	}
	
	public double[] getValue(){
		double returnArray[] = {xValue, yValue};
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
/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.editor.styleeditor.panels;


import javax.swing.event.EventListenerList;

import org.eclipse.swt.*;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.editor.mapeditor.GisMapEditor;
import org.kalypso.editor.styleeditor.dialogs.errordialog.StyleEditorErrorDialog;

/**
 * @author Administrator
 *
 */
public class DenominatorInputPanel {
	
	private Composite parent = null;
	private Composite composite = null;
	private double denominator = 0.0;
	private Text text = null;
	 
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;
	private String label = null;	
	
	
	public DenominatorInputPanel(Composite parent, String label, double denominator){
		this.parent = parent; 
		this.label = label;		
		this.denominator = denominator;
		
		composite = new Composite(parent, SWT.NULL);			
		FormLayout compositeLayout = new FormLayout();
		GridData compositeData = new GridData();
		compositeData.widthHint = 225;
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
		text = new Text(composite,SWT.BORDER);
		text.setBackground(new org.eclipse.swt.graphics.Color(null, new RGB(255,255,255)));		
		
		FormData textData = new FormData();
		textData.height = 10;
		textData.width = 90;	
		textData.left =  new FormAttachment(295, 1000, 0);		
		textData.top =  new FormAttachment(10, 1000, 0);
		text.setLayoutData(textData);		
		text.setText(""+denominator);
		
		Button okButton = new Button(composite,SWT.PUSH);			
		FormData okButtonData = new FormData();
		okButtonData.height = 15;
		okButtonData.width = 22;
		okButtonData.left =  new FormAttachment(900, 1000, 0);		
		okButtonData.top =  new FormAttachment(100, 1000, 0);
		okButton.setLayoutData(okButtonData);		
		okButton.setText("Ok");				
		okButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) 
			{	
				try
				{
					denominator = Double.parseDouble(text.getText());
					if(denominator<0)
					{
						StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog(composite.getShell(),"Input value invalid","needs to be positive");
						errorDialog.showError();
					}
					else
						fire();	
				}
				catch (NumberFormatException nfe) {
					StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog(composite.getShell(),"Input needs to be of type double","needs to be double");
					errorDialog.showError();
					text.setText(""+denominator);
				}																					
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});		
		
		Button getCurrentScaleButton = new Button(composite,SWT.PUSH);			
		FormData getCurrentScaleButtonData = new FormData();
		getCurrentScaleButtonData.height = 15;
		getCurrentScaleButtonData.width = 22;
		getCurrentScaleButtonData.left =  new FormAttachment(770, 1000, 0);		
		getCurrentScaleButtonData.top =  new FormAttachment(100, 1000, 0);
		getCurrentScaleButton.setLayoutData(getCurrentScaleButtonData);		
		getCurrentScaleButton.setText("->");
		getCurrentScaleButton.setToolTipText("aktuellen Maßstab");
		
		getCurrentScaleButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) 
			{
				 IWorkbenchWindow window = Workbench.getInstance().getActiveWorkbenchWindow();    
				 IEditorPart editor = window.getActivePage().getActiveEditor();
				 if(editor instanceof GisMapEditor)
				 {
				 	GisMapEditor gisMapEditor = (GisMapEditor) editor;
				 	denominator = gisMapEditor.getMapPanel().getCurrentScale();				 	
				 }
				text.setText(""+denominator);
				fire();										
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});	
		
		Label urlLabel = new Label(composite,SWT.NULL);					
		FormData urlLabelData = new FormData();
		urlLabelData.height = 15;
		urlLabelData.width = 242;
		urlLabelData.left =  new FormAttachment(0, 1000, 0);		
		urlLabelData.top =  new FormAttachment(100, 1000, 0);
		urlLabel.setLayoutData(urlLabelData);			
		urlLabel.setText(label);
	}
	
	public double getDenominator()
	{
		return denominator;
	}
	
	// sets the inputField to a default state
	public void reset()
	{
		text.setText("");
	}
	
	public void setDenominator(double denom)
	{
		this.denominator = denom;
		if(text != null && !text.isDisposed())
			text.setText(""+denominator);
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
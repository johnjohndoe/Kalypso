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
import org.kalypso.ui.editor.styleeditor.dialogs.StyleEditorErrorDialog;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;

/**
 * @author Administrator
 *
 */
public class RulePatternInputPanel {
	
	private Composite parent = null;
	private Composite composite = null;
	private Text minText = null;
	private Text maxText = null;
	private Text stepText = null;
	
	private double min;
	private double max;
	private double step;
	 
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;
	private String label = null;	
	
	
	public RulePatternInputPanel(Composite parent, String label, double min, double max, double step)
	{
		this.parent = parent; 
		this.label = label;		
		this.min = min; 
		this.max = max;
		this.step = step;
		
		composite = new Composite(parent, SWT.NULL);			
		GridLayout compositeLayout = new GridLayout(4,false);
		GridData compositeData = new GridData();
		compositeData.widthHint = 225;
		composite.setLayoutData(compositeData);
		composite.setLayout(compositeLayout);	
		compositeLayout.marginWidth = 0;
		compositeLayout.marginHeight = 0;		
		composite.layout();			
		init();
	}
	
	public void addPanelListener(PanelListener pl) {
		listenerList.add(PanelListener.class, pl);
	}

	
	private void init()
	{	
		Label urlLabel = new Label(composite,SWT.NULL);					
		GridData urlLabelData = new GridData(62,15);
		urlLabel.setLayoutData(urlLabelData);			
		urlLabel.setText(label);			
		
		Label minLabel = new Label(composite,SWT.NULL);
		minLabel.setText("min:");
				
		minText = new Text(composite,SWT.BORDER);
		minText.setBackground(new org.eclipse.swt.graphics.Color(null, new RGB(255,255,255)));				
		GridData minTextData = new GridData(15,10);
		minText.setLayoutData(minTextData);	
		minText.setText(""+min);
		
		// null placeholder
		new Label(composite,SWT.NULL).setLayoutData(new GridData(50,15));
		
		
		// null placeholder
		new Label(composite,SWT.NULL).setLayoutData(new GridData(50,15));									
		
		Label maxLabel = new Label(composite,SWT.NULL);
		maxLabel.setText("max:");
		
		maxText = new Text(composite,SWT.BORDER);
		maxText.setBackground(new org.eclipse.swt.graphics.Color(null, new RGB(255,255,255)));				
		GridData maxTextData = new GridData(15,10);	
		maxText.setLayoutData(maxTextData);		
		maxText.setText(""+max);
		
		// null placeholder
		new Label(composite,SWT.NULL).setLayoutData(new GridData(50,15));		
		
		// null placeholder
		new Label(composite,SWT.NULL).setLayoutData(new GridData(50,15));		
		
		Label stepLabel = new Label(composite,SWT.NULL);
		stepLabel.setText("step:");		
		
		stepText = new Text(composite,SWT.BORDER);
		stepText.setBackground(new org.eclipse.swt.graphics.Color(null, new RGB(255,255,255)));				
		GridData stepTextData = new GridData(15,10);	
		stepText.setLayoutData(stepTextData);		
		stepText.setText(""+step);		
		
		Button okButton = new Button(composite,SWT.PUSH);					
		GridData okButtonData = new GridData(22,15);		
		okButton.setLayoutData(okButtonData);		
		okButton.setText("Ok");				
		okButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) 
			{	
				try
				{
					double t_min = Double.parseDouble(minText.getText());
					double t_max = Double.parseDouble(maxText.getText());
					double t_step = Double.parseDouble(stepText.getText());
					// check input
					// 1. min < max !!!
					if(t_min>t_max)
					{
						StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog(composite.getShell(),"Input value invalid","Min<Max");
						errorDialog.showError();
					}
					// step>(max-min)
					else if(t_step>(t_max-t_min))
					{
						StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog(composite.getShell(),"Input value invalid","Step cannot be larger than (Max-Min)");
						errorDialog.showError();						
					}
					// step needs to be positive
					else if(t_step<=0)
					{
						StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog(composite.getShell(),"Input value invalid","Step needs to be positive >0");
						errorDialog.showError();						
					}
					else
					{
						min = t_min;
						max = t_max;
						step = t_step;
						fire();	
					}
				}
				catch (NumberFormatException nfe) {
					StyleEditorErrorDialog errorDialog = new StyleEditorErrorDialog(composite.getShell(),"Input needs to be of type double","needs to be double");
					errorDialog.showError();
					minText.setText(""+min);
					maxText.setText(""+max);
					stepText.setText(""+step);					
				}																					
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});						
	}
	
	public double getMax() {
		return max;
	}
	public double getMin() {
		return min;
	}
	public double getStep() {
		return step;
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
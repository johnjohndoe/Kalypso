/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.ui.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.*;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

/**
 * @author Administrator
 *
 */
public class ColorChooserPanel {
	
	private Composite parent = null;
	private Composite composite = null;
	private Color color = new Color(null,255,80,80);
	 
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;
	private String label = null;
	
	
	public ColorChooserPanel(Composite parent, String label, java.awt.Color color2){
		this.parent = parent; 
		this.label = label;
		if(color2 != null)
			color = new Color(null, color2.getRed(), color2.getGreen(), color2.getBlue());
		composite = new Composite(parent, SWT.NULL);			
		FormLayout compositeLayout = new FormLayout();
		GridData compositeData = new GridData();
		compositeData.widthHint = 180;
		composite.setLayoutData(compositeData);
		composite.setLayout(compositeLayout);	
		compositeLayout.marginWidth = 0;
		compositeLayout.marginHeight = 0;
		compositeLayout.spacing = 0;
		composite.layout();			
		init();
	}
	
	public void addColorChooserListener(PanelListener pl) {
		listenerList.add(PanelListener.class, pl);
	}

	
	private void init(){
			
		// Color Image
		final Label fillColorImageInner = new Label(composite,SWT.NULL);	
		FormData fillColorImageInnerData = new FormData();
		fillColorImageInnerData.height = 10;
		fillColorImageInnerData.width = 10;
		fillColorImageInnerData.left =  new FormAttachment(350, 1000, 0);		
		fillColorImageInnerData.top =  new FormAttachment(220, 1000, 0);
		fillColorImageInner.setLayoutData(fillColorImageInnerData);		
		fillColorImageInner.setBackground(color);	
		
		Label fillColorImageOuter = new Label(composite,SWT.NULL);		
		FormData fillColorImageOuterData = new FormData();
		fillColorImageOuterData.height = 14;
		fillColorImageOuterData.width = 14;
		fillColorImageOuterData.left =  new FormAttachment(340, 1000, 0);		
		fillColorImageOuterData.top =  new FormAttachment(120, 1000, 0);
		fillColorImageOuter.setLayoutData(fillColorImageOuterData);		
		fillColorImageOuter.setBackground(new Color(null, 0,0,0));			
		
		Button colorChooserButton = new Button(composite,SWT.PUSH);			
		FormData colorChooserButtonData = new FormData();
		colorChooserButtonData.height = 15;
		colorChooserButtonData.width = 22;
		colorChooserButtonData.left =  new FormAttachment(540, 1000, 0);		
		colorChooserButtonData.top =  new FormAttachment(100, 1000, 0);
		colorChooserButton.setLayoutData(colorChooserButtonData);		
		colorChooserButton.setText("...");
		
		final ColorDialog dialog = new ColorDialog(composite.getShell());
		dialog.setRGB(color.getRGB());
		colorChooserButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				dialog.open();
				color = new Color(null, dialog.getRGB());
				fillColorImageInner.setBackground(color);
				fire();
			}

			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});					
	
		Label fillColorLabel = new Label(composite,SWT.NULL);					
		FormData fillColorLabelLData = new FormData();
		fillColorLabelLData.height = 15;
		fillColorLabelLData.width = 242;
		fillColorLabelLData.left =  new FormAttachment(0, 1000, 0);		
		fillColorLabelLData.top =  new FormAttachment(100, 1000, 0);
		fillColorLabel.setLayoutData(fillColorLabelLData);			
		fillColorLabel.setText(label);
	}
	
	public Color getColor(){
		return color;
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
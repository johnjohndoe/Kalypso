/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.editor.styleeditor.panels;

import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.*;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

/**
 * @author Administrator
 *
 */
public class UrlInputPanel {
	
	private Composite parent = null;
	private Composite composite = null;
	private URL url = null;
	 
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;
	private String label = null;	
	
	
	public UrlInputPanel(Composite parent, String label, URL url){
		this.parent = parent; 
		this.label = label;		
		this.url = url;
		
		composite = new Composite(parent, SWT.NULL);			
		FormLayout compositeLayout = new FormLayout();
		GridData compositeData = new GridData();
		compositeData.widthHint = 195;
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
		final Text text = new Text(composite,SWT.BORDER);
		text.setBackground(new org.eclipse.swt.graphics.Color(null, new RGB(255,255,255)));		
		
		FormData textData = new FormData();
		textData.height = 15;
		textData.width = 90;	
		textData.left =  new FormAttachment(340, 1000, 0);		
		textData.top =  new FormAttachment(10, 1000, 0);
		text.setLayoutData(textData);
		text.setText(url.toString());				
		
		Button okButton = new Button(composite,SWT.PUSH);			
		FormData okButtonData = new FormData();
		okButtonData.height = 15;
		okButtonData.width = 22;
		okButtonData.left =  new FormAttachment(890, 1000, 0);		
		okButtonData.top =  new FormAttachment(100, 1000, 0);
		okButton.setLayoutData(okButtonData);		
		okButton.setText("OK");
		

		okButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) 
			{
				try {
					url = new URL(text.getText());
					fire();
				} catch (MalformedURLException e1) {					
					e1.printStackTrace();
				}							
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
	
	public URL getURL()
	{
		return url;
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
/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

/**
 * @author Administrator
 *
 */
public class ColorBox 
{		 	
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;
	
	private Color color = null;
	
	public ColorBox(Composite parent, Color color2, int size, int borderWidth)
	{		

		if(color2 != null)
			color = color2;
		else
			color = new Color(null,255,80,80);		
		Composite composite = new Composite(parent, SWT.NULL|SWT.BORDER);		
		GridLayout compositeLayout = new GridLayout();
		GridData compositeData = new GridData();
		compositeData.widthHint = size + 2*borderWidth; 
		composite.setLayoutData(compositeData);
		composite.setLayout(compositeLayout);	
		compositeLayout.marginWidth = borderWidth;
		compositeLayout.marginHeight = borderWidth;	
		composite.setBackground(new Color(null, 0,0,0));
		composite.layout();	
		
		final Label fillColorImageInner = new Label(composite,SWT.NULL);
		GridData fillColorImageInnerData = new GridData();		
		fillColorImageInnerData.heightHint = size;
		fillColorImageInnerData.widthHint = size;
		fillColorImageInner.setLayoutData(fillColorImageInnerData);		
		fillColorImageInner.setBackground(color);
		
		final ColorDialog dialog = new ColorDialog(composite.getShell());
		dialog.setRGB(color.getRGB());
		
		composite.addMouseListener(new MouseListener() {
			public void mouseDoubleClick(MouseEvent e) {
				dialog.open();
				color = new Color(null, dialog.getRGB());
				fillColorImageInner.setBackground(color);												
				fire();
			}
			public void mouseDown(MouseEvent e) {
				mouseDoubleClick(e);
			}
			public void mouseUp(MouseEvent e) {				
			}
		});
		fillColorImageInner.addMouseListener(new MouseListener() {
			public void mouseDoubleClick(MouseEvent e) {
				dialog.open();
				color = new Color(null, dialog.getRGB());
				fillColorImageInner.setBackground(color);					
				fire();
			}
			public void mouseDown(MouseEvent e) {
				mouseDoubleClick(e);
			}
			public void mouseUp(MouseEvent e) {				
			}
		});
	}	
	
	public Color getColor()
	{
		return color;
	}
	
	public void addPanelListener(PanelListener pl) {
		listenerList.add(PanelListener.class, pl);
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
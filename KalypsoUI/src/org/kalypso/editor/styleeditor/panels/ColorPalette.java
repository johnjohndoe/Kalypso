/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;

/**
 * @author Administrator
 *
 */
public class ColorPalette {
	
	private Composite parent = null;
	private Composite composite = null;
	 
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;		
	private int colorSize = 0;
	private int borderWidth = 0;
	
	private final int MAX_WIDTH = 5;
	private Color[] colors = null;
	private ColorBox[] colorBoxes = null;
	
	public ColorPalette(Composite parent, Color[] colors, int colorSize, int borderWidth){
		this.parent = parent; 	
		this.colors = colors;
		composite = new Composite(parent, SWT.NULL);		
		GridLayout compositeLayout = new GridLayout(MAX_WIDTH,true);
		GridData compositeData = new GridData();		
		composite.setLayoutData(compositeData);
		composite.setLayout(compositeLayout);	
		compositeLayout.marginWidth = 0;
		compositeLayout.marginHeight = 0;
		compositeLayout.horizontalSpacing = 0;
		compositeLayout.verticalSpacing = 0;
		composite.layout();				
		this.colorSize = colorSize;
		this.borderWidth = borderWidth;
		init();
	}
	
	public void addColorPaletterListener(PanelListener pl) {
		listenerList.add(PanelListener.class, pl);
	}
	
	int i = 0;
	private void init()
	{		
		colorBoxes = new ColorBox[colors.length];		
		for(i=0; i<colors.length; i++)
		{
			final ColorBox box = new ColorBox(composite,colors[i],colorSize,borderWidth);
			colorBoxes[i] = box;
			box.addPanelListener(new PanelListener() {
				public void valueChanged(PanelEvent event) {					
					for(int j=0; j<colorBoxes.length; j++)
						colors[j] = colorBoxes[j].getColor();					
					fire();
				}
			});
		}		
	}
	
	public void setColors(Color[] colors)
	{
		this.colors = colors;
	}
	public Color[] getColors(){
		return colors;
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
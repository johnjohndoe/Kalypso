/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.*;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

/**
 * @author Administrator
 *
 */
public class ColorPalettePanel {
	
	private Composite parent = null;
	private Composite composite = null;
	private Color color = new Color(null,255,80,80);
	 
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;	
	
	public final static int CUSTOM_TRANSITION = -1;
	public final static int RED_GREEN_TRANSITION = 0;
	public final static int BLUE_GREEN_TRANSITION = 1;
	public final static int RED_BLUE_TRANSITION = 2;
	
	private int numberOfColors = 0;
	private final int COLOR_SIZE = 10;
	private final int COLOR_BORDER = 2;
	
	public int type = RED_GREEN_TRANSITION;
	private Color[] customColor = null;
	private Color[] colorArray = null;
	
	private int colorPaletteSelection = 0;
	
	public ColorPalettePanel(Composite parent, Color[] colors, int numberOfColors)
	{
		this.parent = parent; 	
		this.type = RED_GREEN_TRANSITION;
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
		if(colors == null)
			colorArray = initializeColors(type,numberOfColors);
		else 
			colorArray = colors;
		this.numberOfColors = colorArray.length;		
		init();				
	}	
	
	public void addColorPalettePanelListener(PanelListener pl) 
	{		
		listenerList.add(PanelListener.class, pl);		
	}

	public void draw(Composite parent)
	{
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
	private void init()
	{					
		Composite palleteParentComposite = new Composite(composite, SWT.NULL);
		palleteParentComposite.setLayout(new GridLayout());
		FormData palleteParentCompositeData = new FormData();
		palleteParentCompositeData.left =  new FormAttachment(340, 1000, 0);		
		palleteParentCompositeData.top =  new FormAttachment(0, 1000, 0);		
		palleteParentComposite.setLayoutData(palleteParentCompositeData);								
		
		final ColorPaletteComboBox comboBox = new ColorPaletteComboBox(palleteParentComposite);
		comboBox.setSelection(colorPaletteSelection);
		final ColorPalette colorPallete = new ColorPalette(palleteParentComposite,colorArray,COLOR_SIZE,COLOR_BORDER);
		
		comboBox.addPanelListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				switch(comboBox.getSelection())
				{
					case 0: 
					{
						colorArray = initializeColors(RED_GREEN_TRANSITION,numberOfColors);
						break;
					}
					case 1: 
					{
						colorArray = initializeColors(BLUE_GREEN_TRANSITION,numberOfColors);
						break;
					}
					case 2: 
					{
						colorArray = initializeColors(RED_BLUE_TRANSITION,numberOfColors);
						break;
					}
					case 3: 
					{
						if(customColor == null)
							customColor = initializeColors(RED_GREEN_TRANSITION,numberOfColors);
						colorArray = customColor;
						break;
					}
					default:
					{
						colorArray = initializeColors(RED_GREEN_TRANSITION,numberOfColors);
					}
				}
				colorPallete.setColors(colorArray);
				colorPaletteSelection = comboBox.getSelection();
				fire();
			}
		});
				
		colorPallete.addColorPaletterListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				colorArray = colorPallete.getColors();
				fire();
			}
		});
		
		Label fillColorLabel = new Label(composite,SWT.NULL);					
		FormData fillColorLabelLData = new FormData();
		fillColorLabelLData.height = 15;
		fillColorLabelLData.width = 242;
		fillColorLabelLData.left =  new FormAttachment(0, 1000, 0);		
		fillColorLabelLData.top =  new FormAttachment(100, 1000, 0);
		fillColorLabel.setLayoutData(fillColorLabelLData);			
		fillColorLabel.setText("test");
	}
	
	public static Color[] initializeColors(int type,int numberOfColors)
	{
		Color[] colors = new Color[numberOfColors];		
		int step = 255 / numberOfColors;
		for(int i=0; i<numberOfColors; i++)
		{
			if (type == BLUE_GREEN_TRANSITION)
			{
				colors[i] = new Color(null,0,i*step,255-(i*step));
			}
			else if(type == RED_BLUE_TRANSITION)
			{
				colors[i] = new Color(null,255-(i*step),0,i*step);
			}
			else //if(type == RED_GREEN_TRANSITION || type == CUSTOM_TRANSITION)
			{
				colors[i] = new Color(null,255-(i*step),i*step,0);
			}			
		}
		return colors;
	}
	
	public void setColorPalette(Color[] colors)
	{
		colorArray = colors;
	}
	
	public Color[] getColorPalette(){
		return colorArray;
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
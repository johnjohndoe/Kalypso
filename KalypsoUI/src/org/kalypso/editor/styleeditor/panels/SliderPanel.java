/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

/**
 * @author Administrator
 *
 */
public class SliderPanel {
	
	private Composite parent = null;
	private Composite composite = null;	
	 
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;
	private int min = 0; 
	private int max = 100; 
	private int increment = 0;	
	private Text text = null;
	private Slider slider = null;
	
	public final static int DECIMAL = 0;
	public final static int INTEGER = 1;
	private int format = 1;
	private String label = null;
	private double selection = 0.0;
	
	
	public SliderPanel(Composite parent, String label, int minimum, int maximum, int inc, int format, double value){
		this.parent = parent; 		
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
		text = new Text(composite,SWT.READ_ONLY | SWT.BORDER);
		slider = new Slider(composite, SWT.HORIZONTAL);
		
		this.label = label;
		this.format = format;
		this.min = minimum*100; 
		this.max = (maximum-minimum)*100; 
		if(inc>=maximum)
			this.increment = max/10;
		else
			this.increment = inc*100;		
		if(value != -1 && value<=maximum)
			setSelection(value);
		else if(value>maximum)
			setSelection(maximum);				
		init();
	}
	// selection 1-15
	public void setSelection(double selection){
		this.selection = selection;		
		if(format == DECIMAL)
			text.setText(""+selection);
		else
			text.setText(""+(int)selection);
		slider.setSelection((int)(selection*100)-min);		
	}
	
	public void addPanelListener(PanelListener pl) {
		listenerList.add(PanelListener.class, pl);
	}

	
	private void init()
	{		
		text.setBackground(new Color(null, new RGB(255,255,255)));
		
		FormData textData = new FormData();
		textData.height = 10;
		textData.width = 20;
		textData.left =  new FormAttachment(340, 1000, 0);		
		textData.top =  new FormAttachment(120, 1000, 0);
		text.setLayoutData(textData);									
		FormData sliderData = new FormData();
		sliderData.height = 17;
		sliderData.width = 90;
		sliderData.left =  new FormAttachment(540, 1000, 0);
		sliderData.top =  new FormAttachment(100, 1000, 0);
		slider.setLayoutData(sliderData);		
		slider.setIncrement(increment);
		//slider.setMinimum(min);
		slider.setMaximum(max+slider.getThumb());			
		if(format == INTEGER)	
			setSelection((slider.getSelection()+min)/100);
		else
			setSelection((slider.getSelection()+min)/100.0);		
		slider.addMouseListener(new MouseListener() {
			public void mouseDoubleClick(MouseEvent e) {				
			}

			public void mouseDown(MouseEvent e) {	
			}

			public void mouseUp(MouseEvent e) {					
				setSelection((slider.getSelection()+min)/100.00);
				fire();
			}
		});
		slider.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {								
				if(format == INTEGER)					
					text.setText(""+(((Slider)e.getSource()).getSelection()+min)/100);
				else
					text.setText(""+(((Slider)e.getSource()).getSelection()+min)/100.0);					
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
	
    protected void fire() {
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length - 2; i >= 0; i -= 2) {
        	if (listeners[i] == PanelListener.class) {
        		PanelEvent event = new PanelEvent(this);                
                ((PanelListener)listeners[i+1]).valueChanged(event);
        	}
        }
    }	
	
	public double getSelection(){	
		if(format==DECIMAL)
			return selection;
		else
			return (int) selection;
	}
}
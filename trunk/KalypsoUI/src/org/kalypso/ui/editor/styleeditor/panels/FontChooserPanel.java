/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.ui.editor.styleeditor.panels;

import java.awt.Color;

import javax.swing.event.EventListenerList;

import org.deegree.graphics.sld.Font;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.deegree_impl.graphics.sld.StyleFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FontDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * @author Administrator
 *
 */
public class FontChooserPanel {
	
	private Composite parent = null;
	private Composite composite = null;
	private FontData fontData = null;
	private Color color = null;
	private org.eclipse.swt.graphics.Font font = null;
	 
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;
	private String label = null;	
	
	
	public FontChooserPanel(Composite parent, String label, Font font){
		this.parent = parent; 
		this.label = label;		
		try {
			this.font = convertDegreeToSWTFont(font);
			color = font.getColor(null);
		} catch (FilterEvaluationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}		
		composite = new Composite(parent, SWT.NULL);			
		FormLayout compositeLayout = new FormLayout();
		GridData compositeData = new GridData();
		compositeData.widthHint = 200;
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
		final Text text = new Text(composite,SWT.READ_ONLY);
		text.setBackground(new org.eclipse.swt.graphics.Color(null, new RGB(255,255,255)));		
		
		FormData textData = new FormData();
		textData.height = 17;
		textData.width = 25;	
		textData.left =  new FormAttachment(340, 1000, 0);		
		textData.top =  new FormAttachment(10, 1000, 0);
		text.setLayoutData(textData);
		text.setText("abc");
		text.setFont(font);
		text.setForeground(new org.eclipse.swt.graphics.Color(null, color.getRed(), color.getGreen(), color.getBlue()));
		
		Button fontChooserButton = new Button(composite,SWT.PUSH);			
		FormData fontChooserButtonData = new FormData();
		fontChooserButtonData.height = 15;
		fontChooserButtonData.width = 22;
		fontChooserButtonData.left =  new FormAttachment(540, 1000, 0);		
		fontChooserButtonData.top =  new FormAttachment(100, 1000, 0);
		fontChooserButton.setLayoutData(fontChooserButtonData);		
		fontChooserButton.setText("...");
		
		final FontDialog dialog = new FontDialog(composite.getShell());		
		dialog.setFontList(font.getFontData());
		dialog.setRGB(new RGB(color.getRed(), color.getGreen(), color.getBlue()));
		fontChooserButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				fontData = dialog.open();
				RGB rgb = dialog.getRGB();
				color = new Color(rgb.red, rgb.green, rgb.blue);
				if(fontData != null)
				{	
					String name = fontData.getName();
					int style = fontData.getStyle();
					int height = fontData.getHeight();
					if(height>12)
						text.setFont(new org.eclipse.swt.graphics.Font(null,name,12,style));
					else
						text.setFont(new org.eclipse.swt.graphics.Font(null,name,height,style));										
					text.setForeground(new org.eclipse.swt.graphics.Color(null, rgb));				
					fire();
				}
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
	
	public Font getFont(){		
		String fontFamily = fontData.getName();
		boolean italic = false;
		if(fontData.getStyle()> 1)
			italic = true;				
		boolean bold = false;
		if(fontData.getStyle()==1 || fontData.getStyle()==3)
			bold = true;					
		double fontSize = fontData.getHeight();									
		Font font = StyleFactory.createFont(fontFamily, italic,  bold, fontSize);
		font.setColor(getColor());		
		return font;
	}
	
	public static org.eclipse.swt.graphics.Font convertDegreeToSWTFont(Font font) throws FilterEvaluationException
	{
		int style = font.getStyle(null); 
		int weight = font.getWeight(null);
		
		FontData fontData = new FontData();
		fontData.setName(font.getFamily(null));
		fontData.setHeight(font.getSize(null));
		
		if(style== Font.STYLE_NORMAL && weight== Font.WEIGHT_NORMAL)
			fontData.setStyle(SWT.NORMAL);
		else if(style== Font.STYLE_NORMAL && weight== Font.WEIGHT_BOLD)
			fontData.setStyle(SWT.BOLD);
		else if(style== Font.STYLE_ITALIC && weight== Font.WEIGHT_NORMAL)
			fontData.setStyle(SWT.ITALIC);
		else if(style== Font.STYLE_ITALIC && weight== Font.WEIGHT_BOLD)
			fontData.setStyle(SWT.ITALIC|SWT.BOLD);			
				
		return new org.eclipse.swt.graphics.Font(null, fontData);
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
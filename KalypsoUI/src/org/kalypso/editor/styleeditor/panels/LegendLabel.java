/*
 * Created on 19.08.2004
 *
 */
package org.kalypso.editor.styleeditor.panels;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import org.deegree.graphics.Encoders;
import org.deegree.graphics.legend.LegendElement;
import org.deegree.graphics.legend.LegendElementCollection;
import org.deegree.graphics.sld.Rule;
import org.deegree.graphics.sld.UserStyle;
import org.deegree_impl.graphics.legend.LegendElementCollection_Impl;
import org.deegree_impl.graphics.legend.LegendFactory;
import org.deegree_impl.graphics.sld.FeatureTypeStyle_Impl;
import org.deegree_impl.graphics.sld.UserStyle_Impl;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.gml.KalypsoUserStyle;

/**
 * @author Administrator
 *
 */
public 	class LegendLabel implements ModellEventListener, DisposeListener
{
	private Label label = null;
	private KalypsoUserStyle userStyle = null;
	private int ruleIndex = -1;
	private Composite composite = null;
	
	public LegendLabel(Composite parent, KalypsoUserStyle userStyle)
	{
		new LegendLabel(parent, userStyle, -1); 
	}
	
	public LegendLabel(Composite parent, KalypsoUserStyle userStyle, int i)
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
	
		Label legendLabel = new Label(composite,SWT.NULL);					
		FormData legendLabelData = new FormData();
		legendLabelData.height = 15;
		legendLabelData.width = 35;
		legendLabelData.left =  new FormAttachment(0, 1000, 0);		
		legendLabelData.top =  new FormAttachment(150, 1000, 0);
		legendLabel.setLayoutData(legendLabelData);	
		legendLabel.setText("Legend:");											
					
		label = new Label(composite, SWT.NULL);
		FormData labelData = new FormData();					
		labelData.left =  new FormAttachment(340, 1000, 0);
		labelData.top =  new FormAttachment(0, 1000, 0);
		labelData.width = 41;
		label.setLayoutData(labelData);
		
		ruleIndex = i;
		
		this.userStyle = userStyle;
		label.addDisposeListener(this);
		setLegendImage(label, userStyle);							
		userStyle.addModellListener(this);						
	}
	
	public void onModellChange(ModellEvent modellEvent) {
		setLegendImage(label, (UserStyle)modellEvent.getEventSource());			
	}
	
	
	private void setLegendImage(Label label, UserStyle userStyle)
	{
		LegendFactory factory = new LegendFactory();
		try {
			ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
			LegendElement le = null;
			
			if(ruleIndex != -1 && ruleIndex < userStyle.getFeatureTypeStyles()[0].getRules().length)
			{		
				// NECESSARY IF TO SHOW STYLE OF ONLY ONE RULE
                FeatureTypeStyle_Impl fts = new FeatureTypeStyle_Impl();               
                Rule rule = userStyle.getFeatureTypeStyles()[0].getRules()[ruleIndex];
                Rule m_rules[] = { rule };
                fts.setRules(m_rules);
                FeatureTypeStyle_Impl[] ftStyles = {fts};
				UserStyle ruleStyle = new UserStyle_Impl(null,null,null,true,ftStyles);											
				le = factory.createLegendElement(ruleStyle,40,20,"");
				// This is necessary, as I don't want title of the filter to appear in the label but only 
				// an image of the filter itself
				if(le instanceof LegendElementCollection)
				{
					LegendElement elements[] = ((LegendElementCollection_Impl)le).getLegendElements();
					if(elements.length>0)
						le = elements[0];
				}				
			}
			else
			{
				le = factory.createLegendElement(userStyle,40,20,"");				
			}
			
			if(le == null)
				return;
			BufferedImage bi = le.exportAsImage();	
			BufferedImage outbi = new BufferedImage(40,20, BufferedImage.TYPE_INT_ARGB);				
			Graphics g = outbi.getGraphics();
			g.drawImage(bi, 0, 0,Color.WHITE,null);				
			Encoders.encodeGif(outputStream, outbi);			
			ByteArrayInputStream inputStream = new ByteArrayInputStream(outputStream.toByteArray());			
			Image img = new Image(null,inputStream);
			inputStream.read();
			inputStream.close();
			outputStream.close();				
			label.setImage(img);			
		} catch (Exception e1) {
			e1.printStackTrace();
		}						
	}

	public void widgetDisposed(DisposeEvent e) {
		userStyle.removeModellListener(this);			
	}		
}

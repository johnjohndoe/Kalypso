/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.ui.editor.styleeditor.panels;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

/**
 * @author Administrator
 *
 */
public class FormatDisplayPanel {
	
	private Composite parent = null;
	private Composite composite = null;
	private String format = null;
	 
	private PanelEvent panelEvent = null;
	private String label = null;	
	
	
	public FormatDisplayPanel(Composite parent, String label, String format){
		this.parent = parent; 
		this.label = label;		
		this.format = format;
		
		composite = new Composite(parent, SWT.NULL);			
		FormLayout compositeLayout = new FormLayout();
		GridData compositeData = new GridData();
		compositeData.widthHint = 190;
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
		final Text text = new Text(composite,SWT.READ_ONLY);
		//text.setBackground(new org.eclipse.swt.graphics.Color(null, new RGB(255,255,255)));		
		
		FormData textData = new FormData();
		textData.height = 17;
		textData.width = 100;	
		textData.left =  new FormAttachment(340, 1000, 0);		
		textData.top =  new FormAttachment(10, 1000, 0);
		text.setLayoutData(textData);
		text.setText(format);				
							
		Label urlLabel = new Label(composite,SWT.NULL);					
		FormData urlLabelData = new FormData();
		urlLabelData.height = 15;
		urlLabelData.width = 242;
		urlLabelData.left =  new FormAttachment(0, 1000, 0);		
		urlLabelData.top =  new FormAttachment(100, 1000, 0);
		urlLabel.setLayoutData(urlLabelData);			
		urlLabel.setText(label);
	}
	
}
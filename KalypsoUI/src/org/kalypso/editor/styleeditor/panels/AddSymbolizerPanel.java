/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.editor.styleeditor.panels;

import java.awt.Color;
import java.util.ArrayList;

import javax.swing.event.EventListenerList;

import org.deegree.graphics.sld.Graphic;
import org.deegree.graphics.sld.LabelPlacement;
import org.deegree.graphics.sld.Mark;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.graphics.sld.TextSymbolizer;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.graphics.sld.StyleFactory;
import org.eclipse.swt.*;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.kalypso.editor.styleeditor.symbolizerLayouts.TextSymbolizerLayout;

/**
 * @author Administrator
 *
 */
public class AddSymbolizerPanel {
	
	private Composite parent = null;
	private Composite composite = null;
	private FeatureType featureType = null;
	private Combo symbolizerCombo = null;
	private Combo geometryCombo = null;
	private int selectionIndex = 0;
	 
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;
	private String label = null;
	
	
	public AddSymbolizerPanel(Composite parent, String label, FeatureType featureType){
		this.parent = parent; 
		this.label = label;
		this.featureType = featureType;
		
		composite = new Composite(parent, SWT.NULL);			
		FormLayout compositeLayout = new FormLayout();
		GridData compositeData = new GridData();
		compositeData.widthHint = 230;
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
		// Symbolizer Combo
		symbolizerCombo = new Combo(composite,SWT.NULL);
		FormData symbolizerComboData = new FormData();
		symbolizerComboData.height = 21;
		symbolizerComboData.width = 35;
		symbolizerComboData.left =  new FormAttachment(260, 1000, 0);		
		symbolizerComboData.top =  new FormAttachment(100, 1000, 0);
		symbolizerCombo.setLayoutData(symbolizerComboData);								
		String items[] = getItemsByFeatureType(featureType);				
		if(items != null && items.length>0)
		{
			symbolizerCombo.setItems(items);
			symbolizerCombo.select(0);
		}		
		// Geometry-Selection Combo
		geometryCombo = new Combo(composite,SWT.NULL);
		FormData geometryComboData = new FormData();
		geometryComboData.height = 21;
		geometryComboData.width = 35;
		geometryComboData.left =  new FormAttachment(550, 1000, 0);		
		geometryComboData.top =  new FormAttachment(100, 1000, 0);
		geometryCombo.setLayoutData(geometryComboData);	
		String[] geometryItems = getGeometries(featureType);
		if(geometryItems != null && geometryItems.length>0)
		{
			geometryCombo.setItems(geometryItems);
			geometryCombo.select(0);
		}
					
		// Symbolizer Add-Button
		Button symbolizerAddButton = new Button(composite,SWT.PUSH| SWT.CENTER);
		FormData symbolizerAddButtonData = new FormData();
		symbolizerAddButtonData.height = 20;
		symbolizerAddButtonData.width = 30;
		symbolizerAddButtonData.left =  new FormAttachment(860, 1000, 0);		
		symbolizerAddButtonData.top =  new FormAttachment(100, 1000, 0);
		symbolizerAddButton.setLayoutData(symbolizerAddButtonData);
		symbolizerAddButton.setText("Add");
		symbolizerAddButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				setSelection(symbolizerCombo.getSelectionIndex());
				fire();
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});		
		
		// ***** Label
		Label symbolizerLabel = new Label(composite,SWT.NULL);					
		FormData symbolizerLabelData = new FormData();
		symbolizerLabelData.height = 15;
		symbolizerLabelData.width = 242;
		symbolizerLabelData.left =  new FormAttachment(0, 1000, 0);		
		symbolizerLabelData.top =  new FormAttachment(100, 1000, 0);
		symbolizerLabel.setLayoutData(symbolizerLabelData);			
		symbolizerLabel.setText(label);					
	}
	
	public Symbolizer getSelection()
	{				
		String symbolizerString = getItemsByFeatureType(featureType)[selectionIndex]; 		
		if(symbolizerString.equals("Point"))
		{
			Mark mark = StyleFactory.createMark("square");
			Graphic graphic = StyleFactory.createGraphic(null, mark,1.0,2.0,0.0);
			return StyleFactory.createPointSymbolizer(graphic);
		}
		else if(symbolizerString.equals("Line"))					
			return StyleFactory.createLineSymbolizer();
		else if(symbolizerString.equals("Text"))
		{
			TextSymbolizer textSymbolizer = StyleFactory.createTextSymbolizer(geometryCombo.getItem(geometryCombo.getSelectionIndex()),null,null);
			textSymbolizer.setFill(null);
			textSymbolizer.getHalo().getFill().setOpacity(0.3);
			textSymbolizer.setLabel(null);	
			textSymbolizer.getFont().setColor(Color.BLACK);
			LabelPlacement labelPlacement = null;								
			// check which geometry-type
			// if line than label_placement - line_placement
			if(TextSymbolizerLayout.getFeatureTypeGeometryType(featureType) == TextSymbolizerLayout.GM_LINESTRING)
				labelPlacement = StyleFactory.createLabelPlacement(StyleFactory.createLinePlacement("above"));				
			// else label_placement - point_placement
			else if(TextSymbolizerLayout.getFeatureTypeGeometryType(featureType) != TextSymbolizerLayout.GM_LINESTRING)			
				labelPlacement = StyleFactory.createLabelPlacement(StyleFactory.createPointPlacement());
			return textSymbolizer;
		}
		else if(symbolizerString.equals("Polygon"))
			return StyleFactory.createPolygonSymbolizer();
		return null;
	}
	
	public void setSelection(int index)
	{
		selectionIndex = index;		
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
    
    private String[] getGeometries(FeatureType featureType)
    {
    	ArrayList geometryItems = new ArrayList();
    	FeatureTypeProperty[] ftp = featureType.getProperties();
    	for(int i=0; i<ftp.length; i++)    	
    		if(ftp[i].getType().startsWith("org.deegree.model.geometry."))
    			geometryItems.add(ftp[i].getName()); 
    	String returnItems[] = new String[geometryItems.size()];
    	for(int j=0; j<returnItems.length; j++)
    		returnItems[j] = (String)geometryItems.get(j);
    	return returnItems;
    }
    
    private String[] getItemsByFeatureType(FeatureType featureType)
    {
		String items[] = null;		
						
		if(TextSymbolizerLayout.getFeatureTypeGeometryType(featureType)== TextSymbolizerLayout.GM_POINT)
		{
			items = new String[2];
			items[0] = "Point";
			items[1] = "Text";
		}
		else if(TextSymbolizerLayout.getFeatureTypeGeometryType(featureType)== TextSymbolizerLayout.GM_LINESTRING)
		{
			items = new String[3];
			items[0] = "Line";
			items[1] = "Text";
			items[2] = "Point";
		}
		else if(TextSymbolizerLayout.getFeatureTypeGeometryType(featureType)== TextSymbolizerLayout.GM_POLYGON)
		{
			items = new String[3];
			items[0] = "Polygon";
			items[1] = "Text";
			items[2] = "Point";			
		}
		else if(TextSymbolizerLayout.getFeatureTypeGeometryType(featureType)== TextSymbolizerLayout.GM_MULTIPOINT)
		{
			items = new String[2];
			items[0] = "Point";	
			items[1] = "Text";
		}
		else if(TextSymbolizerLayout.getFeatureTypeGeometryType(featureType)== TextSymbolizerLayout.GM_OBJECT) //multilinestring, multipolygon
		{
			items = new String[3];
			items[0] = "Polygon";
			items[1] = "Text";
			items[2] = "Point";	
		}    	
    	return items;    
    }
}
/*
 * Created on 26.07.2004
 *
 */
package org.kalypso.editor.styleeditor.symbolizerLayouts;

import java.util.ArrayList;

import org.deegree.graphics.sld.Fill;
import org.deegree.graphics.sld.Font;
import org.deegree.graphics.sld.Halo;
import org.deegree.graphics.sld.LabelPlacement;
import org.deegree.graphics.sld.ParameterValueType;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.graphics.sld.TextSymbolizer;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.services.wfs.filterencoding.Expression;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.deegree_impl.graphics.sld.StyleFactory;
import org.deegree_impl.services.wfs.filterencoding.PropertyName;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.editor.styleeditor.panels.ColorChooserPanel;
import org.kalypso.editor.styleeditor.panels.FontChooserPanel;
import org.kalypso.editor.styleeditor.panels.LabelPlacementComboPanel;
import org.kalypso.editor.styleeditor.panels.LabelPointPlacementPanel;
import org.kalypso.editor.styleeditor.panels.TextInputPanel;
import org.kalypso.editor.styleeditor.panels.PanelEvent;
import org.kalypso.editor.styleeditor.panels.PanelListener;
import org.kalypso.editor.styleeditor.panels.SliderPanel;
import org.kalypso.editor.styleeditor.panels.TextLabelComboPanel;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.gml.KalypsoUserStyle;

/**
 * @author Administrator
 *
 */

public class TextSymbolizerLayout extends SymbolizerLayout{	
	
	private Fill polygonFill = null;
	private FeatureType featureType = null;
	private TextInputPanel labelTextInput = null;
	private TextLabelComboPanel textLabelComboPanel = null;
	private Halo halo = null;
	private LabelPlacement labelPlacement = null;
	
	public final static int GM_POINT = 0;
	public final static int GM_LINESTRING = 1;
	public final static int GM_POLYGON = 2;
	public final static int GM_MULTIPOINT = 3;
	public final static int GM_OBJECT = 4;	
	
	public TextSymbolizerLayout(Composite composite, Symbolizer symbolizer, KalypsoUserStyle userStyle, FeatureType featureType)
	{
		super(composite,symbolizer,userStyle);
		this.featureType = featureType;
	}
		
	public void draw() throws FilterEvaluationException
	{		
		final TextSymbolizer textSymbolizer = (TextSymbolizer) symbolizer;
		
		GridLayout compositeLayout = new GridLayout();		
		compositeLayout.marginHeight = 2;		
			
		// ***** Font group
		Group fontGroup = new Group(composite,SWT.NULL);		
		GridData fontGroupData = new GridData();
		fontGroupData.widthHint = 210;			
		fontGroup.setLayoutData(fontGroupData);			
		fontGroup.setLayout(compositeLayout);
		fontGroup.layout();				 		
		
		// check whether already a Label-source in sld
		ParameterValueType label = textSymbolizer.getLabel();
		String labelTextCombo = null;
		String labelTextField = null;
		if(label != null)
		{
			Object obj[] = label.getComponents();
			for(int i=0; i<obj.length; i++)
			{
				if(obj[i] instanceof PropertyName)
				{
					labelTextCombo = ((PropertyName)obj[i]).getValue().trim();					
					break;
				}
				else if(obj[i] instanceof String)
				{
					String labelString = ((String)obj[i]).trim();
					if(labelString.length()>0)
					{
						labelTextField = labelString;
						break;
					}
				}					
			}
		}
		textLabelComboPanel = new TextLabelComboPanel(fontGroup, "Label:",featureType,labelTextCombo);
		labelTextInput = new TextInputPanel(fontGroup,"Or text:",labelTextField);
			
		textLabelComboPanel.addPanelListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				String ftpString = ((TextLabelComboPanel)event.getSource()).getSelectedFeatureTypeProperty();
				PropertyName propName = new PropertyName(ftpString);
				Expression exp[] = {propName};
				textSymbolizer.setLabel(StyleFactory.createParameterValueType(exp));				
				labelTextInput.reset();
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
			}
		});	
		labelTextInput.addPanelListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				String labelText = ((TextInputPanel)event.getSource()).getLabelText();
				textSymbolizer.setLabel(StyleFactory.createParameterValueType(labelText));
				textLabelComboPanel.reset();
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
			}
		});
		
		Font font = textSymbolizer.getFont(); 					
		FontChooserPanel fontChooserPanel = new FontChooserPanel(fontGroup, "Font:",font);
		fontChooserPanel.addPanelListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				FontChooserPanel source = (FontChooserPanel)event.getSource();
				Font font = source.getFont();				
				textSymbolizer.setFont(font);								
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));				
			}
		});
			
		
// 		***** Halo Group
		
//		Group haloGroup = new Group(composite,SWT.NULL);
//		haloGroup.setText("Halo");
//		GridData haloGroupData = new GridData();
//		haloGroupData.widthHint = 210;			
//		haloGroup.setLayoutData(haloGroupData);			
//		haloGroup.setLayout(compositeLayout);
//		haloGroup.layout();	
		
		halo = textSymbolizer.getHalo();
		if(halo == null)
		{
			halo = StyleFactory.createHalo();
			halo.getFill().setOpacity(0.3);			
		}
		// Halo ColorChooser
		ColorChooserPanel haloColorChooserPanel = null;
		haloColorChooserPanel = new ColorChooserPanel(fontGroup, "Color:", halo.getFill().getFill(null));		
		haloColorChooserPanel.addColorChooserListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				Color color = ((ColorChooserPanel) event.getSource()).getColor();			
				halo.getFill().setFill(new java.awt.Color(color.getRed(), color.getGreen(), color.getBlue()));
				if(textSymbolizer.getHalo() == null)
					textSymbolizer.setHalo(halo);
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
			}
		});				
		
		// Halo Opacity Slider
		SliderPanel haloOpacityPanel = null;
		haloOpacityPanel = new SliderPanel(fontGroup, "Opacity:", 0,1,1,SliderPanel.DECIMAL,halo.getFill().getOpacity(null));	
		haloOpacityPanel.addPanelListener(new PanelListener() {			
			public void valueChanged(PanelEvent event) {
				double opacity = ((SliderPanel)event.getSource()).getSelection();
				halo.getFill().setOpacity(opacity);
				if(textSymbolizer.getHalo() == null)
					textSymbolizer.setHalo(halo);				
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
			}
		});	
		
		// Halo Stroke Opacity Slider
		SliderPanel haloStrokeOpacityPanel = null;
		haloStrokeOpacityPanel = new SliderPanel(fontGroup, "Stroke Opacity:", 0,1,1,SliderPanel.DECIMAL,halo.getStroke().getOpacity(null));	
		haloStrokeOpacityPanel.addPanelListener(new PanelListener() {			
			public void valueChanged(PanelEvent event) {
				double opacity = ((SliderPanel)event.getSource()).getSelection();
				halo.getStroke().setOpacity(opacity);
				if(textSymbolizer.getHalo() == null)
					textSymbolizer.setHalo(halo);				
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
			}
		});			
		
		labelPlacement = textSymbolizer.getLabelPlacement();
		if(getFeatureTypeGeometryType(featureType) == GM_LINESTRING)
		{	
			if(labelPlacement == null)
				labelPlacement = StyleFactory.createLabelPlacement(StyleFactory.createLinePlacement("auto"));
			int linePlacementIndex = labelPlacement.getLinePlacement().getPlacementType(null);					
			LabelPlacementComboPanel labelPlacementComboBoxPanel = new LabelPlacementComboPanel(fontGroup,"Placement:", linePlacementIndex);
			labelPlacementComboBoxPanel.addPanelListener(new PanelListener() {
				public void valueChanged(PanelEvent event) {
					int type = ((LabelPlacementComboPanel)event.getSource()).getSelection();
					labelPlacement.getLinePlacement().setPlacementType(type);
					if(textSymbolizer.getLabelPlacement() == null)
						textSymbolizer.setLabelPlacement(labelPlacement);					
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
				}
			});	
			
			SliderPanel gapPanel = new SliderPanel(fontGroup, "Gap:", 0,10,1,SliderPanel.INTEGER,labelPlacement.getLinePlacement().getGap(null));	
			gapPanel.addPanelListener(new PanelListener() {			
				public void valueChanged(PanelEvent event) {
					double gap = ((SliderPanel)event.getSource()).getSelection();					
					labelPlacement.getLinePlacement().setGap((int)gap);
					if(textSymbolizer.getLabelPlacement() == null)
						textSymbolizer.setLabelPlacement(labelPlacement);				
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));					
				}
			});	
			
		}	
		else
		{
			if(labelPlacement == null)
				labelPlacement = StyleFactory.createLabelPlacement(StyleFactory.createPointPlacement(0.0,0.0,0.0,0.0,0.0));
			
			double displacement[] = labelPlacement.getPointPlacement().getDisplacement(null);			
			LabelPointPlacementPanel labelPointPlacementPanel = new LabelPointPlacementPanel(fontGroup,"Placement:",displacement);
			labelPointPlacementPanel.addPanelListener(new PanelListener() {
				public void valueChanged(PanelEvent event) {
					double disp[] = ((LabelPointPlacementPanel)event.getSource()).getValue();
					labelPlacement.getPointPlacement().setDisplacement(disp);
					if(textSymbolizer.getLabelPlacement() == null)
						textSymbolizer.setLabelPlacement(labelPlacement);
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));					
				}
			});
			
			SliderPanel rotationPanel = rotationPanel = new SliderPanel(fontGroup, "Rotation:", 0,360,15,SliderPanel.INTEGER,labelPlacement.getPointPlacement().getRotation(null)*180);	
			rotationPanel.addPanelListener(new PanelListener() {			
				public void valueChanged(PanelEvent event) {
					double rotation = ((SliderPanel)event.getSource()).getSelection();
					rotation = rotation/180.0;
					labelPlacement.getPointPlacement().setRotation(rotation);
					if(textSymbolizer.getLabelPlacement() == null)
						textSymbolizer.setLabelPlacement(labelPlacement);				
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));					
				}
			});	
		}
	}
	
    public static int getFeatureTypeGeometryType(FeatureType featureType)
    {    	    
    	String ft = null;
    	// get the Geometry Name
    	ArrayList geometryItems = new ArrayList();
    	FeatureTypeProperty[] ftp = featureType.getProperties();
    	for(int i=0; i<ftp.length; i++)    	
    		if(ftp[i].getType().startsWith("org.deegree.model.geometry."))
    			geometryItems.add(ftp[i].getType()); 
    	String geometries[] = new String[geometryItems.size()];
    	for(int j=0; j<geometries.length; j++)
    		geometries[j] = (String)geometryItems.get(j);
    	
    	if(geometries.length>0)
    		ft = geometries[0];
    	if(ft == null)
    		return -1;    	
    	if(ft.equals("org.deegree.model.geometry.GM_Point"))
    		return GM_POINT;
		else if(ft.equals("org.deegree.model.geometry.GM_LineString"))
			return GM_LINESTRING;
		else if(ft.equals("org.deegree.model.geometry.GM_Polygon"))
			return GM_POLYGON;
		else if(ft.equals("org.deegree.model.geometry.GM_MultiPoint"))
			return GM_MULTIPOINT;
		else if(ft.equals("org.deegree.model.geometry.GM_Object")) //multilinestring, multipolygon
			return GM_OBJECT;    	
    	return -1;
    }	
}

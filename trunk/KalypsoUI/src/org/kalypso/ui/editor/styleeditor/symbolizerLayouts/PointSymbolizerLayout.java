/*
 * Created on 26.07.2004
 *
 */
package org.kalypso.ui.editor.styleeditor.symbolizerLayouts;


import java.net.URL;

import org.deegree.graphics.sld.ExternalGraphic;
import org.deegree.graphics.sld.Fill;
import org.deegree.graphics.sld.Graphic;
import org.deegree.graphics.sld.Mark;
import org.deegree.graphics.sld.PointSymbolizer;
import org.deegree.graphics.sld.Stroke;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.deegree_impl.graphics.sld.StyleFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.panels.ColorChooserPanel;
import org.kalypso.ui.editor.styleeditor.panels.ComboPanel;
import org.kalypso.ui.editor.styleeditor.panels.ConfigurePointSymbolizerPanel;
import org.kalypso.ui.editor.styleeditor.panels.FormatDisplayPanel;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;
import org.kalypso.ui.editor.styleeditor.panels.SliderPanel;
import org.kalypso.ui.editor.styleeditor.panels.UrlInputPanel;
import org.kalypso.ui.editor.styleeditor.panels.WellKnownNameComboPanel;

/**
 * @author Administrator
 *
 */

public class PointSymbolizerLayout extends SymbolizerLayout{	
	
	private int markCounter = 1;
	private int extGraphicCounter = 1;
	private Composite tabFolderComposite = null;
	private int selectionIndex = 0;
	
	public PointSymbolizerLayout(Composite composite, Symbolizer symbolizer, KalypsoUserStyle userStyle)
	{
		super(composite,symbolizer,userStyle);
	}
	
	public void draw() throws FilterEvaluationException
	{
		final PointSymbolizer pointSymbolizer = (PointSymbolizer) symbolizer;
		final Graphic graphic = pointSymbolizer.getGraphic();
		markCounter = 1;
		extGraphicCounter = 1;		
				
		if(tabFolderComposite != null)
			tabFolderComposite.dispose();
		tabFolderComposite = new Composite(composite, SWT.NULL);		
		tabFolderComposite.setLayout(new FormLayout());			
		tabFolderComposite.layout();
		
		final TabFolder markExtGraphicTabFolder = new TabFolder(tabFolderComposite,SWT.NULL);		
		FormData markExtGraphicTabFolderData = new FormData();
		markExtGraphicTabFolderData.height = 160;
		markExtGraphicTabFolderData.width = 208;
		markExtGraphicTabFolderData.left =  new FormAttachment(10, 1000, 0);		
		markExtGraphicTabFolderData.top =  new FormAttachment(20, 1000, 0);								
		markExtGraphicTabFolder.setLayoutData(markExtGraphicTabFolderData);						
					
		// ***** Graphic-Detail Section after TabFolder
		Composite graphicDetailComposite = new Composite(tabFolderComposite, SWT.NULL);
		FormLayout formLayout = new FormLayout();
		graphicDetailComposite.setLayout(formLayout);
		FormData labelData = new FormData();
		labelData.height = 110;
		labelData.width = 198;
		labelData.left =  new FormAttachment(72, 1000, 0);		
		labelData.top =  new FormAttachment(650, 1000, 0);		
		graphicDetailComposite.setLayoutData(labelData);	
		
		Composite graphicDetails = new Composite(graphicDetailComposite,SWT.NULL);
		GridLayout graphicDetailsLayout = new GridLayout();
		graphicDetails.setLayout(graphicDetailsLayout);		
		graphicDetailsLayout.marginHeight = 0;
		graphicDetailsLayout.marginWidth = 0;
		graphicDetails.layout();						

		
		final Object objects[]  = graphic.getMarksAndExtGraphics();		
		
		for(int i=0; i<objects.length; i++)
		{			
			drawTabItem(markExtGraphicTabFolder,objects[i]);
		}
		if(selectionIndex<objects.length)
			markExtGraphicTabFolder.setSelection(selectionIndex);
		
		SliderPanel graphicSizePanel = new SliderPanel(graphicDetails, "Size:", 1,15,1,SliderPanel.INTEGER,graphic.getSize(null));
		graphicSizePanel.addPanelListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				double size = ((SliderPanel)event.getSource()).getSelection();
				graphic.setSize(size);
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));				
			}
		});

		SliderPanel graphicOpacityPanel = new SliderPanel(graphicDetails, "Opacity:", 0,1,1,SliderPanel.DECIMAL,graphic.getOpacity(null));
		graphicOpacityPanel.addPanelListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				double opacity = ((SliderPanel)event.getSource()).getSelection();
				graphic.setOpacity(opacity);
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));				
			}
		});
		
		SliderPanel rotationPanel = new SliderPanel(graphicDetails, "Rotation:", 0,360,15,SliderPanel.INTEGER,graphic.getRotation(null)*180);
		rotationPanel.addPanelListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {
				double rotation = ((SliderPanel)event.getSource()).getSelection();
				rotation = rotation/180.0;
				graphic.setRotation(rotation);
				userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));				
			}
		});		
	
		ConfigurePointSymbolizerPanel configurePointSymbolizerPanel = new ConfigurePointSymbolizerPanel(graphicDetails, objects.length);		
		configurePointSymbolizerPanel.addPanelListener(new PanelListener() {
			public void valueChanged(PanelEvent event) {				
				int action = ((ConfigurePointSymbolizerPanel)event.getSource()).getAction();
				if(action == ConfigurePointSymbolizerPanel.ADD_MARK)
				{								
					try {
						Mark newMark = StyleFactory.createMark("square");
						pointSymbolizer.getGraphic().addMarksAndExtGraphic(newMark);	
						selectionIndex = graphic.getMarksAndExtGraphics().length-1;
						draw();
						userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));						
					} catch (FilterEvaluationException e) {						
						e.printStackTrace();
					}				
				}
				else if(action == ConfigurePointSymbolizerPanel.REM_MARK)
				{					
					int index = markExtGraphicTabFolder.getSelectionIndex();
					if(objects.length >0)
					{
						Object object = objects[index];
						pointSymbolizer.getGraphic().removeMarksAndExtGraphic(object);
						selectionIndex = index;
						if(selectionIndex<0)
							selectionIndex = 0;
						userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
						try {
							draw();
						} catch (FilterEvaluationException e) {							
							e.printStackTrace();
						}						
					}					
				}
				else if(action == ConfigurePointSymbolizerPanel.FOR_MARK)
				{					
					int index = markExtGraphicTabFolder.getSelectionIndex();
					if(index == (objects.length-1))
						return;
					
					Object newOrderedObjects[] = new Object[objects.length];
					for(int i=0; i<objects.length; i++)
					{
						if(i == index)
							newOrderedObjects[i] = objects[i+1];
						else if(i == (index+1))
							newOrderedObjects[i] = objects[i-1];
						else
							newOrderedObjects[i] = objects[i];											
					}
					graphic.setMarksAndExtGraphics(newOrderedObjects);
					selectionIndex = index+1;
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
					try {
						draw();						
					} catch (FilterEvaluationException e) { 
						e.printStackTrace();
					}
				}
				else if(action == ConfigurePointSymbolizerPanel.BAK_MARK)
				{					
					int index = markExtGraphicTabFolder.getSelectionIndex();
					if(index == 0)
						return;
					
					Object newOrderedObjects[] = new Object[objects.length];
					for(int i=0; i<objects.length; i++)
					{
						if(i == index)
							newOrderedObjects[i] = objects[i-1];
						else if(i == (index-1))
							newOrderedObjects[i] = objects[i+1];
						else
							newOrderedObjects[i] = objects[i];											
					}
					graphic.setMarksAndExtGraphics(newOrderedObjects);
					selectionIndex = index-1;
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
					try {
						draw();						
					} catch (FilterEvaluationException e) { 
						e.printStackTrace();
					}
				}				
			}
		});
		
		tabFolderComposite.pack(true);
	}
	
	private void drawTabItem(TabFolder markExtGraphicTabFolder, Object object) throws FilterEvaluationException
	{
		TabItem tabItem = new TabItem(markExtGraphicTabFolder, SWT.NULL);
		Composite tabItemComposite = new Composite(markExtGraphicTabFolder,SWT.NULL);		
		
		GridLayout compositeLayout = new GridLayout();
		tabItemComposite.setLayout(compositeLayout);		
		compositeLayout.marginHeight = 0;
		compositeLayout.marginWidth = 0;
		tabItemComposite.layout();				
		tabItem.setControl(tabItemComposite);	
						
		// ***** Fill Group
		
		GridLayout groupLayout = new GridLayout();		
		groupLayout.marginHeight = 2;
		
		Group group = new Group(tabItemComposite, SWT.NULL);
		group.setText("");				
		GridData fillGroupData = new GridData();
		fillGroupData.widthHint = 202;
		group.setLayoutData(fillGroupData);	
		group.setLayout(groupLayout);
		group.layout();	

		// ***** Mark or External-Graphic Group		
		if(object instanceof Mark)
		{
			final Mark mark = (Mark) object;
			tabItem.setText("Mark"+(markCounter++));
			ComboPanel wellKnownNameComboBox = new WellKnownNameComboPanel(group,"Type",mark.getWellKnownName());
			wellKnownNameComboBox.addPanelListener(new PanelListener() {
				public void valueChanged(PanelEvent event) {
					int index = ((ComboPanel)event.getSource()).getSelection();
					mark.setWellKnownName(WellKnownNameComboPanel.getWellKnownNameByIndex(index));						
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
				}
			});	
			
			final Fill markFill;
			if(mark.getFill() == null)
				markFill = StyleFactory.createFill();
			else
				markFill = mark.getFill();
			ColorChooserPanel fillColorChooserPanel = new ColorChooserPanel(group, "Fill-Color:", markFill.getFill(null));
			fillColorChooserPanel.addColorChooserListener(new PanelListener() {
				public void valueChanged(PanelEvent event) { 
					Color color = ((ColorChooserPanel) event.getSource()).getColor();			
					markFill.setFill(new java.awt.Color(color.getRed(), color.getGreen(), color.getBlue()));				
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));				
				}
			});				
			
			SliderPanel fillOpacityPanel = new SliderPanel(group, "Fill-Opacity", 0,1,1,SliderPanel.DECIMAL,markFill.getOpacity(null));		
			fillOpacityPanel.addPanelListener(new PanelListener() {
				public void valueChanged(PanelEvent event) {				
					double opacity = ((SliderPanel)event.getSource()).getSelection();
					markFill.setOpacity(opacity);
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
				}
			});	
			
			final Stroke markStroke;
			if(mark.getStroke() == null)
				markStroke = StyleFactory.createStroke();
			else
				markStroke = mark.getStroke();
			ColorChooserPanel strokeColorChooserPanel = new ColorChooserPanel(group, "Color:",markStroke.getStroke(null));
			strokeColorChooserPanel.addColorChooserListener(new PanelListener() {
				public void valueChanged(PanelEvent event) {
					Color color = ((ColorChooserPanel) event.getSource()).getColor();
					markStroke.setStroke(new java.awt.Color(color.getRed(), color.getGreen(), color.getBlue()));
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
				}
			});
			
			SliderPanel strokeWidthPanel = new SliderPanel(group, "width:", 0,10,1,SliderPanel.INTEGER,markStroke.getWidth(null));
			strokeWidthPanel.addPanelListener(new PanelListener() {
				public void valueChanged(PanelEvent event) {
					double width = ((SliderPanel)event.getSource()).getSelection();
					markStroke.setWidth(width);
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));				
				}
			});
	
			SliderPanel strokeOpacityPanel = new SliderPanel(group, "Opacity:", 0,1,1,SliderPanel.DECIMAL,markStroke.getOpacity(null));
			strokeOpacityPanel.addPanelListener(new PanelListener() {
				public void valueChanged(PanelEvent event) {
					double opacity = ((SliderPanel)event.getSource()).getSelection();
					markStroke.setOpacity(opacity);
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));					
				}
			});				
		}
		else if(object instanceof ExternalGraphic)
		{
			final ExternalGraphic externalGraphic = (ExternalGraphic) object;				
			tabItem.setText("ExtGraph"+(extGraphicCounter++));
			UrlInputPanel urlInputPanel = new UrlInputPanel(group,"URL:", externalGraphic.getOnlineResource());
			urlInputPanel.addPanelListener(new PanelListener() {
				public void valueChanged(PanelEvent event) {
					URL url = ((UrlInputPanel)event.getSource()).getURL();
					externalGraphic.setOnlineResource(url);	
					userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));
				}
			});
			FormatDisplayPanel formatDisplayPanel = new FormatDisplayPanel(group, "Format:", externalGraphic.getFormat());																					
		}		
	}
}

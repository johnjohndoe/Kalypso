/*
 * Created on 26.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.symbolizerLayouts;

import org.deegree.graphics.sld.Fill;
import org.deegree.graphics.sld.PolygonSymbolizer;
import org.deegree.graphics.sld.Stroke;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.deegree_impl.graphics.sld.StyleFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.panels.ColorChooserPanel;
import org.kalypso.ui.editor.styleeditor.panels.ComboPanel;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;
import org.kalypso.ui.editor.styleeditor.panels.SliderPanel;
import org.kalypso.ui.editor.styleeditor.panels.StrokeDasharrayPanel;
import org.kalypso.ui.editor.styleeditor.panels.StrokeDashoffsetPanel;
import org.kalypso.ui.editor.styleeditor.panels.StrokeLinecapComboPanel;
import org.kalypso.ui.editor.styleeditor.panels.StrokeLinejoinComboPanel;

/**
 * @author F.Lindemann
 *  
 */

public class PolygonSymbolizerLayout extends AbstractSymbolizerLayout
{

  private Fill polygonFill = null;

  public PolygonSymbolizerLayout( Composite m_composite, Symbolizer m_symbolizer,
      KalypsoUserStyle m_userStyle )
  {
    super( m_composite, m_symbolizer, m_userStyle );
  }

  public void draw() throws FilterEvaluationException
  {
    PolygonSymbolizer polygonSymbolizer = (PolygonSymbolizer)symbolizer;

    GridLayout compositeLayout = new GridLayout();
    compositeLayout.marginHeight = 2;

    // ***** Fill Group
    Group fillGroup = new Group( composite, SWT.NULL );
    fillGroup.setText( "Fill" );
    GridData fillGroupData = new GridData();
    fillGroupData.widthHint = 210;
    fillGroup.setLayoutData( fillGroupData );
    fillGroup.setLayout( compositeLayout );
    fillGroup.layout();

    polygonFill = polygonSymbolizer.getFill();
    if( polygonFill == null )
    {
      polygonFill = StyleFactory.createFill( java.awt.Color.WHITE, 0.0 );
      polygonSymbolizer.setFill( polygonFill );
    }
    ColorChooserPanel fillColorChooserPanel = new ColorChooserPanel( fillGroup, "Fill-Color:",
        polygonFill.getFill( null ) );
    fillColorChooserPanel.addColorChooserListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        Color color = ( (ColorChooserPanel)event.getSource() ).getColor();
        getPolygonFill().setFill(
            new java.awt.Color( color.getRed(), color.getGreen(), color.getBlue() ) );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    SliderPanel fillOpacityPanel = new SliderPanel( fillGroup, "Fill-Opacity", 0, 1, 1,
        SliderPanel.DECIMAL, polygonFill.getOpacity( null ) );
    fillOpacityPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        double opacity = ( (SliderPanel)event.getSource() ).getSelection();
        getPolygonFill().setOpacity( opacity );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    // ***** Stroke Group
    compositeLayout = new GridLayout();
    compositeLayout.marginHeight = 2;

    Group strokeGroup = new Group( composite, SWT.NULL );
    strokeGroup.setText( "Stroke" );
    GridData strokeGroupData = new GridData();
    strokeGroupData.widthHint = 210;
    strokeGroupData.heightHint = 179;
    strokeGroup.setLayoutData( strokeGroupData );
    strokeGroup.setLayout( compositeLayout );
    strokeGroup.layout();

    final Stroke polygonStroke = polygonSymbolizer.getStroke();
    ColorChooserPanel strokeColorChooserPanel = new ColorChooserPanel( strokeGroup, "Color:",
        polygonStroke.getStroke( null ) );
    strokeColorChooserPanel.addColorChooserListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        Color color = ( (ColorChooserPanel)event.getSource() ).getColor();
        polygonStroke.setStroke( new java.awt.Color( color.getRed(), color.getGreen(), color
            .getBlue() ) );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    SliderPanel strokeWidthPanel = new SliderPanel( strokeGroup, "width:", 0, 10, 1,
        SliderPanel.INTEGER, polygonStroke.getWidth( null ) );
    strokeWidthPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        double width = ( (SliderPanel)event.getSource() ).getSelection();
        polygonStroke.setWidth( width );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    SliderPanel strokeOpacityPanel = new SliderPanel( strokeGroup, "Opacity:", 0, 1, 1,
        SliderPanel.DECIMAL, polygonStroke.getOpacity( null ) );
    strokeOpacityPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        double opacity = ( (SliderPanel)event.getSource() ).getSelection();
        polygonStroke.setOpacity( opacity );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    // Stroke Linejoin ComboPanel
    ComboPanel strokeLinejoinPanel = new StrokeLinejoinComboPanel( strokeGroup, "Linejoin",
        polygonStroke.getLineJoin( null ) );
    polygonStroke.setLineJoin( strokeLinejoinPanel.getSelection() );
    strokeLinejoinPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        int lineJoin = ( (StrokeLinejoinComboPanel)event.getSource() ).getSelection();
        polygonStroke.setLineJoin( lineJoin );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    // Stroke Linecap ComboPanel
    ComboPanel strokeLinecapPanel = new StrokeLinecapComboPanel( strokeGroup, "Linecap",
        polygonStroke.getLineCap( null ) );
    polygonStroke.setLineCap( strokeLinecapPanel.getSelection() );
    strokeLinecapPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        int lineCap = ( (StrokeLinecapComboPanel)event.getSource() ).getSelection();
        polygonStroke.setLineCap( lineCap );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    // Stroke DashOffset
    StrokeDashoffsetPanel strokeDashoffsetPanel = new StrokeDashoffsetPanel( strokeGroup,
        "Dashoffset", polygonStroke.getDashOffset( null ) );
    strokeDashoffsetPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        float dashOffset = ( (StrokeDashoffsetPanel)event.getSource() ).getValue();
        polygonStroke.setDashOffset( dashOffset );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    // Stroke DashArray
    StrokeDasharrayPanel strokeDasharrayPanel = new StrokeDasharrayPanel( strokeGroup, "Dasharray",
        polygonStroke.getDashArray( null ) );
    strokeDasharrayPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        float dashArray[] = ( (StrokeDasharrayPanel)event.getSource() ).getValue();
        polygonStroke.setDashArray( dashArray );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );
  }

  public Fill getPolygonFill()
  {
    return polygonFill;
  }

  public void setPolygonFill( Fill m_polygonFill )
  {
    this.polygonFill = m_polygonFill;
  }
}
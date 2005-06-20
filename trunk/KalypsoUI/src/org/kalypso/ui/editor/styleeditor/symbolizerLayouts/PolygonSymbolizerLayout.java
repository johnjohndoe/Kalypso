/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
/*
 * Created on 26.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.symbolizerLayouts;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.MessageBundle;
import org.kalypso.ui.editor.styleeditor.panels.ColorChooserPanel;
import org.kalypso.ui.editor.styleeditor.panels.ComboPanel;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;
import org.kalypso.ui.editor.styleeditor.panels.SliderPanel;
import org.kalypso.ui.editor.styleeditor.panels.StrokeDasharrayPanel;
import org.kalypso.ui.editor.styleeditor.panels.StrokeDashoffsetPanel;
import org.kalypso.ui.editor.styleeditor.panels.StrokeLinecapComboPanel;
import org.kalypso.ui.editor.styleeditor.panels.StrokeLinejoinComboPanel;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;

/**
 * @author F.Lindemann
 *  
 */

public class PolygonSymbolizerLayout extends AbstractSymbolizerLayout
{

  private Fill polygonFill = null;

  public PolygonSymbolizerLayout( Composite m_composite, Symbolizer m_symbolizer, KalypsoUserStyle m_userStyle )
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
    fillGroup.setText( MessageBundle.STYLE_EDITOR_FILL );
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
    ColorChooserPanel fillColorChooserPanel = new ColorChooserPanel( fillGroup, MessageBundle.STYLE_EDITOR_FILL_COLOR,
        polygonFill.getFill( null ) );
    fillColorChooserPanel.addColorChooserListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        Color color = ( (ColorChooserPanel)event.getSource() ).getColor();
        getPolygonFill().setFill( new java.awt.Color( color.getRed(), color.getGreen(), color.getBlue() ) );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    SliderPanel fillOpacityPanel = new SliderPanel( fillGroup, MessageBundle.STYLE_EDITOR_FILL_OPACITY, 0, 1, 1,
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
    strokeGroup.setText( MessageBundle.STYLE_EDITOR_STROKE );
    GridData strokeGroupData = new GridData();
    strokeGroupData.widthHint = 210;
    strokeGroupData.heightHint = 179;
    strokeGroup.setLayoutData( strokeGroupData );
    strokeGroup.setLayout( compositeLayout );
    strokeGroup.layout();

    final Stroke polygonStroke = polygonSymbolizer.getStroke();
    ColorChooserPanel strokeColorChooserPanel = new ColorChooserPanel( strokeGroup, MessageBundle.STYLE_EDITOR_COLOR,
        polygonStroke.getStroke( null ) );
    strokeColorChooserPanel.addColorChooserListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        Color color = ( (ColorChooserPanel)event.getSource() ).getColor();
        polygonStroke.setStroke( new java.awt.Color( color.getRed(), color.getGreen(), color.getBlue() ) );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    SliderPanel strokeWidthPanel = new SliderPanel( strokeGroup, MessageBundle.STYLE_EDITOR_WIDTH, 0, 10, 1,
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

    SliderPanel strokeOpacityPanel = new SliderPanel( strokeGroup, MessageBundle.STYLE_EDITOR_OPACITY, 0, 1, 1,
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
    ComboPanel strokeLinejoinPanel = new StrokeLinejoinComboPanel( strokeGroup, MessageBundle.STYLE_EDITOR_LINEJOIN,
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
    ComboPanel strokeLinecapPanel = new StrokeLinecapComboPanel( strokeGroup, MessageBundle.STYLE_EDITOR_LINECAP,
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
        MessageBundle.STYLE_EDITOR_DASHOFFSET, polygonStroke.getDashOffset( null ) );
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
    StrokeDasharrayPanel strokeDasharrayPanel = new StrokeDasharrayPanel( strokeGroup,
        MessageBundle.STYLE_EDITOR_DASHARRAY, polygonStroke.getDashArray( null ) );
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
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
 * Created on 15.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.panels;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import javax.swing.event.EventListenerList;

import org.deegree.graphics.sld.Graphic;
import org.deegree.graphics.sld.Mark;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.graphics.sld.TextSymbolizer;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.graphics.sld.StyleFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.styleeditor.MessageBundle;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.TextSymbolizerLayout;

/**
 * @author F.Lindemann
 *  
 */
public class AddSymbolizerPanel
{

  private final Composite m_composite;

  private final FeatureType m_featureType;

  private Combo m_symbolizerCombo = null;

  private Combo m_geometryCombo = null;

  private final EventListenerList m_listenerList = new EventListenerList();

  private final String m_label;

  private final boolean m_isSimpleRule;

  /**
   *  
   */
  public AddSymbolizerPanel( Composite parent, String label, FeatureType featureType )
  {
    m_label = label;
    m_featureType = featureType;
    m_composite = new Composite( parent, SWT.NULL );
    m_isSimpleRule = true;
    FormLayout compositeLayout = new FormLayout();
    GridData compositeData = new GridData();
    compositeData.widthHint = 230;
    m_composite.setLayoutData( compositeData );
    m_composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    compositeLayout.spacing = 0;
    m_composite.layout();
    init();
  }

  public AddSymbolizerPanel( Composite parent, String label, FeatureType featureType,
      boolean isSimpleRule )
  {
    m_label = label;
    m_featureType = featureType;
    m_isSimpleRule = isSimpleRule;
    m_composite = new Composite( parent, SWT.NULL );
    FormLayout compositeLayout = new FormLayout();
    GridData compositeData = new GridData();
    compositeData.widthHint = 230;
    m_composite.setLayoutData( compositeData );
    m_composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    compositeLayout.spacing = 0;
    m_composite.layout();
    init();
  }

  public void addPanelListener( PanelListener pl )
  {
    m_listenerList.add( PanelListener.class, pl );
  }

  private void init()
  {
    // Symbolizer Combo
    m_symbolizerCombo = new Combo( m_composite, SWT.NULL );
    final FormData symbolizerComboData = new FormData();
    symbolizerComboData.height = 21;
    symbolizerComboData.width = 30;
    symbolizerComboData.left = new FormAttachment( 295, 1000, 0 );
    symbolizerComboData.top = new FormAttachment( 100, 1000, 0 );
    m_symbolizerCombo.setLayoutData( symbolizerComboData );

    // Geometry-Selection Combo
    m_geometryCombo = new Combo( m_composite, SWT.NULL );
    final FormData geometryComboData = new FormData();
    geometryComboData.height = 21;
    geometryComboData.width = 35;
    geometryComboData.left = new FormAttachment( 560, 1000, 0 );
    geometryComboData.top = new FormAttachment( 100, 1000, 0 );
    m_geometryCombo.setLayoutData( geometryComboData );
    m_geometryCombo.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        updateSymbolizerCombo();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        updateSymbolizerCombo();
      }
    } );

    List geometryItems = queryGeometriesPropertyNames( m_featureType.getProperties(), null );
    geometryItems = queryGeometriesPropertyNames( m_featureType.getVirtuelFeatureTypeProperty(),
        geometryItems );
    if( geometryItems != null && geometryItems.size() > 0 )
    {
      m_geometryCombo
          .setItems( (String[])geometryItems.toArray( new String[geometryItems.size()] ) );
      m_geometryCombo.select( 0 );
    }

    // Symbolizer Add-Button
    Label symbolizerAddButton = new Label( m_composite, SWT.PUSH | SWT.CENTER );
    symbolizerAddButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_ADD_RULE.createImage() );
    FormData symbolizerAddButtonData = new FormData();
    symbolizerAddButtonData.height = 20;
    symbolizerAddButtonData.width = 30;
    symbolizerAddButtonData.left = new FormAttachment( 860, 1000, 0 );
    symbolizerAddButtonData.top = new FormAttachment( 100, 1000, 0 );
    symbolizerAddButton.setLayoutData( symbolizerAddButtonData );
    symbolizerAddButton.setToolTipText( MessageBundle.STYLE_EDITOR_ADD );
    symbolizerAddButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( MouseEvent e )
      {     
        fire();
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {
      // nothing
      }

    } );

    // ***** Label
    Label symbolizerLabel = new Label( m_composite, SWT.NULL );
    FormData symbolizerLabelData = new FormData();
    symbolizerLabelData.height = 15;
    symbolizerLabelData.width = 242;
    symbolizerLabelData.left = new FormAttachment( 0, 1000, 0 );
    symbolizerLabelData.top = new FormAttachment( 100, 1000, 0 );
    symbolizerLabel.setLayoutData( symbolizerLabelData );
    symbolizerLabel.setText( m_label );
  }

  protected void updateSymbolizerCombo()
  {
    int selectionIndex = m_geometryCombo.getSelectionIndex();
    String propName = m_geometryCombo.getItem( selectionIndex );
    final String items[] = getSymbolizerTypesByFeatureProperty( propName );
    if( items != null && items.length > 0 )
    {
      m_symbolizerCombo.setItems( items );
      m_symbolizerCombo.select( 0 );
    }

  }

  public Symbolizer getSelection()
  {
    String geometryPropertyName = m_geometryCombo.getItem( m_geometryCombo.getSelectionIndex() );
    int selectionIndex = m_symbolizerCombo.getSelectionIndex();
    String symbolizerString = m_symbolizerCombo.getItem( selectionIndex );
    //    String symbolizerString =
    // getSymbolizerTypesByFeatureProperty()[m_selectionIndex];
    return getSymbolizer( geometryPropertyName, symbolizerString, m_featureType );
  }

  public static Symbolizer getSymbolizer( String geometryPropertyName, String symbolizerString,
      FeatureType featureType )
  {
    FeatureTypeProperty ftp=featureType.getProperty(geometryPropertyName);
    if( symbolizerString.equals( "Point" ) )
    {
      Mark mark = StyleFactory.createMark( "square" );
      Graphic graphic = StyleFactory.createGraphic( null, mark, 1.0, 2.0, 0.0 );
      return StyleFactory.createPointSymbolizer( graphic, geometryPropertyName );
    }
    else if( symbolizerString.equals( "Line" ) )
    {
      return StyleFactory.createLineSymbolizer( StyleFactory.createStroke(), geometryPropertyName );
    }
    else if( symbolizerString.equals( "Text" ) )
    {
      TextSymbolizer textSymbolizer = StyleFactory.createTextSymbolizer( geometryPropertyName,
          null, null );
      textSymbolizer.setFill( null );
      textSymbolizer.getHalo().getFill().setOpacity( 0.3 );
      textSymbolizer.setLabel( null );
      textSymbolizer.getFont().setColor( Color.BLACK );
      // check which geometry-type
      // if line than label_placement - line_placement
      if( TextSymbolizerLayout.getFeatureTypeGeometryType( ftp ) == TextSymbolizerLayout.GM_LINESTRING )
        StyleFactory.createLabelPlacement( StyleFactory.createLinePlacement( "above" ) );
      // else label_placement - point_placement
      else
        StyleFactory.createLabelPlacement( StyleFactory.createPointPlacement() );
      return textSymbolizer;
    }
    else if( symbolizerString.equals( "Polygon" ) )
      return StyleFactory.createPolygonSymbolizer( StyleFactory.createStroke(), StyleFactory
          .createFill(), geometryPropertyName );
    return null;
  }

  protected void fire()
  {
    final Object[] listeners = m_listenerList.getListenerList();
    for( int i = listeners.length - 2; i >= 0; i -= 2 )
    {
      if( listeners[i] == PanelListener.class )
      {
        PanelEvent event = new PanelEvent( this );
        ( (PanelListener)listeners[i + 1] ).valueChanged( event );
      }
    }
  }

  public static List queryGeometriesPropertyNames( FeatureTypeProperty[] ftp, List list )
  {
    if( list == null )
      list = new ArrayList();

    for( int i = 0; i < ftp.length; i++ )
    {
      String type = ftp[i].getType();
      if( type.startsWith( "org.deegree.model.geometry." ) && !type.endsWith( "GM_Envelope" ) )
        list.add( ftp[i].getName() );
    }
    return list;
  }

  private String[] getSymbolizerTypesByFeatureProperty( String propName )
  {
    FeatureTypeProperty ftp=m_featureType.getProperty(propName);
    if(ftp==null)
      ftp=m_featureType.getVirtuelFeatureTypeProperty(propName);
    String items[] = null;
    // in case of Pattern-Rule it does not make sense to have a pattern for
    // textsymbolizer

    if( TextSymbolizerLayout.getFeatureTypeGeometryType( ftp ) == TextSymbolizerLayout.GM_POINT )
    {
      if( m_isSimpleRule )
      {
        items = new String[2];
        items[0] = "Point";
        items[1] = "Text";
      }
      else
      {
        items = new String[1];
        items[0] = "Point";
      }
    }
    else if( TextSymbolizerLayout.getFeatureTypeGeometryType( ftp ) == TextSymbolizerLayout.GM_LINESTRING )
    {
      if( m_isSimpleRule )
      {
        items = new String[3];
        items[0] = "Line";
        items[1] = "Text";
        items[2] = "Point";
      }
      else
      {
        items = new String[2];
        items[0] = "Line";
        items[1] = "Point";
      }
    }
    else if( TextSymbolizerLayout.getFeatureTypeGeometryType(ftp ) == TextSymbolizerLayout.GM_POLYGON )
    {
      if( m_isSimpleRule )
      {
        items = new String[3];
        items[0] = "Polygon";
        items[1] = "Text";
        items[2] = "Point";
      }
      else
      {
        items = new String[2];
        items[0] = "Polygon";
        items[1] = "Point";
      }
    }
    else if( TextSymbolizerLayout.getFeatureTypeGeometryType( ftp ) == TextSymbolizerLayout.GM_MULTIPOINT )
    {
      if( m_isSimpleRule )
      {
        items = new String[2];
        items[0] = "Point";
        items[1] = "Text";
      }
      else
      {
        items = new String[1];
        items[0] = "Point";
      }
    }
    else if( TextSymbolizerLayout.getFeatureTypeGeometryType( ftp ) == TextSymbolizerLayout.GM_OBJECT ) //multilinestring,
    // multipolygon
    {
      if( m_isSimpleRule )
      {
        items = new String[3];
        items[0] = "Polygon";
        items[1] = "Text";
        items[2] = "Point";
      }
      else
      {
        items = new String[2];
        items[0] = "Polygon";
        items[1] = "Point";
      }
    }
    return items;
  }

  public Combo getSymbolizerCombo()
  {
    return m_symbolizerCombo;
  }
}
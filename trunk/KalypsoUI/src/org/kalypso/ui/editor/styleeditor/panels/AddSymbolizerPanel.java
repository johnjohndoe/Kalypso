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
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.styleeditor.MessageBundle;
import org.kalypso.ui.editor.styleeditor.StyleEditorHelper;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.TextSymbolizerLayout;
import org.kalypsodeegree.graphics.sld.Graphic;
import org.kalypsodeegree.graphics.sld.Mark;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.TextSymbolizer;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualPropertyUtilities;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author F.Lindemann
 */
public class AddSymbolizerPanel
{

  private final Composite m_composite;

  private final IFeatureType m_featureType;

  private Combo m_symbolizerCombo = null;

  private Combo m_geometryCombo = null;

  private final EventListenerList m_listenerList = new EventListenerList();

  private final String m_label;

  private final boolean m_isSimpleRule;

  public AddSymbolizerPanel( final Composite parent, final String label, final IFeatureType featureType )
  {
    m_label = label;
    m_featureType = featureType;
    m_composite = new Composite( parent, SWT.NULL );
    m_isSimpleRule = true;
    final FormLayout compositeLayout = new FormLayout();
    final GridData compositeData = new GridData();
    compositeData.widthHint = 230;
    m_composite.setLayoutData( compositeData );
    m_composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    compositeLayout.spacing = 0;
    m_composite.layout();
    init();
  }

  public AddSymbolizerPanel( final Composite parent, final String label, final IFeatureType featureType, final boolean isSimpleRule )
  {
    m_label = label;
    m_featureType = featureType;
    m_isSimpleRule = isSimpleRule;
    m_composite = new Composite( parent, SWT.NULL );
    final FormLayout compositeLayout = new FormLayout();
    final GridData compositeData = new GridData();
    compositeData.widthHint = 230;
    m_composite.setLayoutData( compositeData );
    m_composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    compositeLayout.spacing = 0;
    m_composite.layout();
    init();
  }

  public void addPanelListener( final PanelListener pl )
  {
    m_listenerList.add( PanelListener.class, pl );
  }

  private void init( )
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
      public void widgetSelected( final SelectionEvent e )
      {
        updateSymbolizerCombo();
      }

      public void widgetDefaultSelected( final SelectionEvent e )
      {
        updateSymbolizerCombo();
      }
    } );

    List<String> geometryItems = queryGeometriesPropertyNames( m_featureType.getProperties(), null );
    geometryItems = queryGeometriesPropertyNames( VirtualPropertyUtilities.getVirtualProperties( m_featureType ), geometryItems );
    if( geometryItems != null && geometryItems.size() > 0 )
    {
      m_geometryCombo.setItems( geometryItems.toArray( new String[geometryItems.size()] ) );
      m_geometryCombo.select( 0 );
    }

    // Symbolizer Add-Button
    final Label symbolizerAddButton = new Label( m_composite, SWT.PUSH | SWT.CENTER );
    symbolizerAddButton.setImage( ImageProvider.IMAGE_STYLEEDITOR_ADD_RULE.createImage() );
    final FormData symbolizerAddButtonData = new FormData();
    symbolizerAddButtonData.height = 20;
    symbolizerAddButtonData.width = 30;
    symbolizerAddButtonData.left = new FormAttachment( 860, 1000, 0 );
    symbolizerAddButtonData.top = new FormAttachment( 100, 1000, 0 );
    symbolizerAddButton.setLayoutData( symbolizerAddButtonData );
    symbolizerAddButton.setToolTipText( MessageBundle.STYLE_EDITOR_ADD );
    symbolizerAddButton.addMouseListener( new MouseListener()
    {
      public void mouseDoubleClick( final MouseEvent e )
      {
        fire();
      }

      public void mouseDown( final MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( final MouseEvent e )
      {
        // nothing
      }

    } );

    // ***** Label
    final Label symbolizerLabel = new Label( m_composite, SWT.NULL );
    final FormData symbolizerLabelData = new FormData();
    symbolizerLabelData.height = 15;
    symbolizerLabelData.width = 242;
    symbolizerLabelData.left = new FormAttachment( 0, 1000, 0 );
    symbolizerLabelData.top = new FormAttachment( 100, 1000, 0 );
    symbolizerLabel.setLayoutData( symbolizerLabelData );
    symbolizerLabel.setText( m_label );

    updateSymbolizerCombo();
  }

  protected void updateSymbolizerCombo( )
  {
    final int selectionIndex = m_geometryCombo.getSelectionIndex();
    if( selectionIndex == -1 )
      return;
    final String propName = m_geometryCombo.getItem( selectionIndex );
    final String items[] = getSymbolizerTypesByFeatureProperty( new PropertyName( propName ) );
    if( items != null && items.length > 0 )
    {
      m_symbolizerCombo.setItems( items );
      m_symbolizerCombo.select( 0 );
    }
  }

  public Symbolizer getSelection( )
  {
    final String geometryPropertyName = m_geometryCombo.getItem( m_geometryCombo.getSelectionIndex() );
    final int selectionIndex = m_symbolizerCombo.getSelectionIndex();
    final String symbolizerString = m_symbolizerCombo.getItem( selectionIndex );
    // String symbolizerString =
    // getSymbolizerTypesByFeatureProperty()[m_selectionIndex];
    return getSymbolizer( new PropertyName( geometryPropertyName ), symbolizerString, m_featureType );
  }

  public static Symbolizer getSymbolizer( final PropertyName geometryPropertyName, final String symbolizerString, final IFeatureType featureType )
  {
    final IPropertyType ftp = StyleEditorHelper.getFeatureTypeProperty( featureType, geometryPropertyName );

    if( symbolizerString.equals( "Point" ) ) //$NON-NLS-1$
    {
      final Mark mark = StyleFactory.createMark( "square" ); //$NON-NLS-1$
      final Graphic graphic = StyleFactory.createGraphic( null, mark, 1.0, 2.0, 0.0 );
      return StyleFactory.createPointSymbolizer( graphic, geometryPropertyName );
    }
    else if( symbolizerString.equals( "Line" ) ) //$NON-NLS-1$
    {
      return StyleFactory.createLineSymbolizer( StyleFactory.createStroke(), geometryPropertyName );
    }
    else if( symbolizerString.equals( "Text" ) ) //$NON-NLS-1$
    {

      final TextSymbolizer textSymbolizer = StyleFactory.createTextSymbolizer( geometryPropertyName, null, null );
      textSymbolizer.setFill( null );
      textSymbolizer.getHalo().getFill().setOpacity( 0.3 );
      textSymbolizer.setLabel( null );
      textSymbolizer.getFont().setColor( Color.BLACK );
      // check which geometry-type
      // if line than label_placement - line_placement
      if( TextSymbolizerLayout.getFeatureTypeGeometryType( ftp ) == TextSymbolizerLayout.GM_LINESTRING )
        StyleFactory.createLabelPlacement( StyleFactory.createLinePlacement( "above" ) ); //$NON-NLS-1$
      // else label_placement - point_placement
      else
        StyleFactory.createLabelPlacement( StyleFactory.createPointPlacement() );
      return textSymbolizer;
    }
    else if( symbolizerString.equals( "Polygon" ) ) //$NON-NLS-1$
      return StyleFactory.createPolygonSymbolizer( StyleFactory.createStroke(), StyleFactory.createFill(), geometryPropertyName );
    return null;
  }

  protected void fire( )
  {
    final Object[] listeners = m_listenerList.getListenerList();
    for( int i = listeners.length - 2; i >= 0; i -= 2 )
    {
      if( listeners[i] == PanelListener.class )
      {
        final PanelEvent event = new PanelEvent( this );
        ((PanelListener) listeners[i + 1]).valueChanged( event );
      }
    }
  }

  public static List<String> queryGeometriesPropertyNames( final IPropertyType[] ftp, List<String> list )
  {
    if( list == null )
      list = new ArrayList<String>();

    for( final IPropertyType element : ftp )
    {
      if( GeometryUtilities.isGeometry( element ) )
        list.add( element.getName() );
    }
    return list;
  }

  private String[] getSymbolizerTypesByFeatureProperty( final PropertyName propName )
  {

    final IPropertyType ftp = StyleEditorHelper.getFeatureTypeProperty( m_featureType, propName );

    String items[] = null;
    // in case of Pattern-Rule it does not make sense to have a pattern for
    // textsymbolizer

    final int featureTypeGeometryType = TextSymbolizerLayout.getFeatureTypeGeometryType( ftp );
    if( featureTypeGeometryType == TextSymbolizerLayout.GM_POINT || featureTypeGeometryType == TextSymbolizerLayout.GM_MULTIPOINT )
    {
      if( m_isSimpleRule )
      {
        items = new String[2];
        items[0] = "Point"; //$NON-NLS-1$
        items[1] = "Text"; //$NON-NLS-1$
      }
      else
      {
        items = new String[1];
        items[0] = "Point"; //$NON-NLS-1$
      }
    }
    else if( featureTypeGeometryType == TextSymbolizerLayout.GM_LINESTRING || featureTypeGeometryType == TextSymbolizerLayout.GM_MULTILINESTRING )
    {
      if( m_isSimpleRule )
      {
        items = new String[3];
        items[0] = "Line"; //$NON-NLS-1$
        items[1] = "Text"; //$NON-NLS-1$
        items[2] = "Point"; //$NON-NLS-1$
      }
      else
      {
        items = new String[2];
        items[0] = "Line"; //$NON-NLS-1$
        items[1] = "Point"; //$NON-NLS-1$
      }
    }
    else if( featureTypeGeometryType == TextSymbolizerLayout.GM_POLYGON || featureTypeGeometryType == TextSymbolizerLayout.GM_MULTIPOLYGON )
    {
      if( m_isSimpleRule )
      {
        items = new String[3];
        items[0] = "Polygon"; //$NON-NLS-1$
        items[1] = "Text"; //$NON-NLS-1$
        items[2] = "Point"; //$NON-NLS-1$
      }
      else
      {
        items = new String[2];
        items[0] = "Polygon"; //$NON-NLS-1$
        items[1] = "Point"; //$NON-NLS-1$
      }
    }
    else if( featureTypeGeometryType == TextSymbolizerLayout.GM_OBJECT )
    {
      if( m_isSimpleRule )
      {
        items = new String[4];
        items[0] = "Text"; //$NON-NLS-1$
        items[1] = "Point"; //$NON-NLS-1$
        items[2] = "Line"; //$NON-NLS-1$
        items[3] = "Polygon"; //$NON-NLS-1$
      }
      else
      {
        items = new String[3];
        items[0] = "Point"; //$NON-NLS-1$
        items[1] = "Line"; //$NON-NLS-1$
        items[2] = "Polygon"; //$NON-NLS-1$
      }
    }
    return items;
  }

  public Combo getSymbolizerCombo( )
  {
    return m_symbolizerCombo;
  }
}
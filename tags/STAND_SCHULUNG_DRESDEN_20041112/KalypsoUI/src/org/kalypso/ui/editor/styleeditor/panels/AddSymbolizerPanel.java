/*
 * Created on 15.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.panels;

import java.awt.Color;
import java.util.ArrayList;

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

  private Composite composite = null;

  private FeatureType featureType = null;

  private Combo symbolizerCombo = null;

  private Combo geometryCombo = null;

  private int selectionIndex = 0;

  private EventListenerList listenerList = new EventListenerList();

  private String label = null;

  private boolean isSimpleRule = true;

  public AddSymbolizerPanel( Composite parent, String m_label, FeatureType m_featureType )
  {
    setLabel( m_label );
    setFeatureType( m_featureType );
    composite = new Composite( parent, SWT.NULL );
    FormLayout compositeLayout = new FormLayout();
    GridData compositeData = new GridData();
    compositeData.widthHint = 230;
    composite.setLayoutData( compositeData );
    composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    compositeLayout.spacing = 0;
    composite.layout();
    init();
  }

  public AddSymbolizerPanel( Composite parent, String m_label, FeatureType m_featureType,
      boolean m_isSimpleRule )
  {
    setLabel( m_label );
    setFeatureType( m_featureType );
    this.isSimpleRule = m_isSimpleRule;
    composite = new Composite( parent, SWT.NULL );
    FormLayout compositeLayout = new FormLayout();
    GridData compositeData = new GridData();
    compositeData.widthHint = 230;
    composite.setLayoutData( compositeData );
    composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    compositeLayout.spacing = 0;
    composite.layout();
    init();
  }

  public void addPanelListener( PanelListener pl )
  {
    listenerList.add( PanelListener.class, pl );
  }

  private void init()
  {
    // Symbolizer Combo
    symbolizerCombo = new Combo( composite, SWT.NULL );
    FormData symbolizerComboData = new FormData();
    symbolizerComboData.height = 21;
    symbolizerComboData.width = 30;
    symbolizerComboData.left = new FormAttachment( 295, 1000, 0 );
    symbolizerComboData.top = new FormAttachment( 100, 1000, 0 );
    symbolizerCombo.setLayoutData( symbolizerComboData );
    String items[] = getItemsByFeatureType( featureType );
    if( items != null && items.length > 0 )
    {
      symbolizerCombo.setItems( items );
      symbolizerCombo.select( 0 );
    }
    // Geometry-Selection Combo
    geometryCombo = new Combo( composite, SWT.NULL );
    FormData geometryComboData = new FormData();
    geometryComboData.height = 21;
    geometryComboData.width = 35;
    geometryComboData.left = new FormAttachment( 560, 1000, 0 );
    geometryComboData.top = new FormAttachment( 100, 1000, 0 );
    geometryCombo.setLayoutData( geometryComboData );
    String[] geometryItems = getGeometries( featureType );
    if( geometryItems != null && geometryItems.length > 0 )
    {
      geometryCombo.setItems( geometryItems );
      geometryCombo.select( 0 );
    }

    // Symbolizer Add-Button
    Label symbolizerAddButton = new Label( composite, SWT.PUSH | SWT.CENTER );
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
        setSelection( getSymbolizerCombo().getSelectionIndex() );
        fire();
      }

      public void mouseDown( MouseEvent e )
      {
        mouseDoubleClick( e );
      }

      public void mouseUp( MouseEvent e )
      {/**/}
    } );

    // ***** Label
    Label symbolizerLabel = new Label( composite, SWT.NULL );
    FormData symbolizerLabelData = new FormData();
    symbolizerLabelData.height = 15;
    symbolizerLabelData.width = 242;
    symbolizerLabelData.left = new FormAttachment( 0, 1000, 0 );
    symbolizerLabelData.top = new FormAttachment( 100, 1000, 0 );
    symbolizerLabel.setLayoutData( symbolizerLabelData );
    symbolizerLabel.setText( label );
  }

  public Symbolizer getSelection()
  {
    String geometryPropertyName = geometryCombo.getItem( geometryCombo.getSelectionIndex() );
    String symbolizerString = getItemsByFeatureType( featureType )[selectionIndex];
    return getSymbolizer( geometryPropertyName, symbolizerString, featureType );
  }

  public static Symbolizer getSymbolizer( String geometryPropertyName, String symbolizerString,
      FeatureType featureType )
  {
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
      if( TextSymbolizerLayout.getFeatureTypeGeometryType( featureType ) == TextSymbolizerLayout.GM_LINESTRING )
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

  public void setSelection( int index )
  {
    this.selectionIndex = index;
  }

  protected void fire()
  {
    Object[] listeners = listenerList.getListenerList();
    for( int i = listeners.length - 2; i >= 0; i -= 2 )
    {
      if( listeners[i] == PanelListener.class )
      {
        PanelEvent event = new PanelEvent( this );
        ( (PanelListener)listeners[i + 1] ).valueChanged( event );
      }
    }
  }

  public static String[] getGeometries( FeatureType featureType )
  {
    ArrayList geometryItems = new ArrayList();
    FeatureTypeProperty[] ftp = featureType.getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      String type = ftp[i].getType();
      if( type.startsWith( "org.deegree.model.geometry." ) && !type.endsWith( "GM_Envelope" ) )
        geometryItems.add( ftp[i].getName() );
    }
    String returnItems[] = new String[geometryItems.size()];
    for( int j = 0; j < returnItems.length; j++ )
      returnItems[j] = (String)geometryItems.get( j );
    return returnItems;
  }

  private String[] getItemsByFeatureType( FeatureType m_featureType )
  {
    String items[] = null;
    // in case of Pattern-Rule it does not make sense to have a pattern for
    // textsymbolizer
    if( TextSymbolizerLayout.getFeatureTypeGeometryType( m_featureType ) == TextSymbolizerLayout.GM_POINT )
    {
      if( isSimpleRule )
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
    else if( TextSymbolizerLayout.getFeatureTypeGeometryType( m_featureType ) == TextSymbolizerLayout.GM_LINESTRING )
    {
      if( isSimpleRule )
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
    else if( TextSymbolizerLayout.getFeatureTypeGeometryType( m_featureType ) == TextSymbolizerLayout.GM_POLYGON )
    {
      if( isSimpleRule )
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
    else if( TextSymbolizerLayout.getFeatureTypeGeometryType( m_featureType ) == TextSymbolizerLayout.GM_MULTIPOINT )
    {
      if( isSimpleRule )
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
    else if( TextSymbolizerLayout.getFeatureTypeGeometryType( m_featureType ) == TextSymbolizerLayout.GM_OBJECT ) //multilinestring,
    // multipolygon
    {
      if( isSimpleRule )
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

  public FeatureType getFeatureType()
  {
    return featureType;
  }

  public void setFeatureType( FeatureType m_featureType )
  {
    this.featureType = m_featureType;
  }

  public Combo getGeometryCombo()
  {
    return geometryCombo;
  }

  public void setGeometryCombo( Combo m_geometryCombo )
  {
    this.geometryCombo = m_geometryCombo;
  }

  public String getLabel()
  {
    return label;
  }

  public void setLabel( String m_label )
  {
    this.label = m_label;
  }

  public int getSelectionIndex()
  {
    return selectionIndex;
  }

  public void setSelectionIndex( int m_selectionIndex )
  {
    this.selectionIndex = m_selectionIndex;
  }

  public Combo getSymbolizerCombo()
  {
    return symbolizerCombo;
  }

  public void setSymbolizerCombo( Combo m_symbolizerCombo )
  {
    this.symbolizerCombo = m_symbolizerCombo;
  }
}
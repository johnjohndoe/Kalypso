/*
 * Created on 15.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.panels;

import java.util.ArrayList;

import javax.swing.event.EventListenerList;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.services.wfs.filterencoding.PropertyName;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.ui.editor.styleeditor.dialogs.filterencoding.BoundaryExpression;

/**
 * @author F.Lindemann
 *  
 */
public class AddPatternPanel
{

  private Composite composite = null;

  private FeatureType featureType = null;

  private Combo symbolizerCombo = null;

  private Combo geometryCombo = null;

  private int selectionIndex = 0;

  private EventListenerList listenerList = new EventListenerList();

  private String label = null;

  public AddPatternPanel( Composite parent, String m_label, FeatureType m_featureType )
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

  public void addPanelListener( PanelListener pl )
  {
    listenerList.add( PanelListener.class, pl );
  }

  private void init()
  {
    // Geometry-Selection Combo
    geometryCombo = new Combo( composite, SWT.NULL );
    FormData geometryComboData = new FormData();
    geometryComboData.height = 21;
    geometryComboData.width = 75;
    geometryComboData.left = new FormAttachment( 260, 1000, 0 );
    geometryComboData.top = new FormAttachment( 100, 1000, 0 );
    geometryCombo.setLayoutData( geometryComboData );
    String[] geometryItems = getGeometries( featureType );
    if( geometryItems != null && geometryItems.length > 0 )
    {
      geometryCombo.setItems( geometryItems );
      geometryCombo.select( 0 );
    }

    // Symbolizer Add-Button
    Button symbolizerAddButton = new Button( composite, SWT.PUSH | SWT.CENTER );
    FormData symbolizerAddButtonData = new FormData();
    symbolizerAddButtonData.height = 20;
    symbolizerAddButtonData.width = 30;
    symbolizerAddButtonData.left = new FormAttachment( 860, 1000, 0 );
    symbolizerAddButtonData.top = new FormAttachment( 100, 1000, 0 );
    symbolizerAddButton.setLayoutData( symbolizerAddButtonData );
    symbolizerAddButton.setText( "Add" );
    symbolizerAddButton.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        setSelection( getSymbolizerCombo().getSelectionIndex() );
        fire();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );
      }
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
    String items[] = null;
    ArrayList list = new ArrayList();
    FeatureTypeProperty[] ftp = featureType.getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      if( ftp[i].getType().equalsIgnoreCase( "java.lang.Double" ) )
        list.add( ftp[i] );
      else if( ftp[i].getType().equalsIgnoreCase( "java.math.BigInteger" ) )
        list.add( ftp[i] );
      else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Byte" ) )
        list.add( ftp[i] );
      else if( ftp[i].getType().equalsIgnoreCase( "java.math.BigDecimal" ) )
        list.add( ftp[i] );
      else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Float" ) )
        list.add( ftp[i] );
      else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Integer" ) )
        list.add( ftp[i] );
      else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Long" ) )
        list.add( ftp[i] );
      else if( ftp[i].getType().equalsIgnoreCase( "java.lang.Short" ) )
        list.add( ftp[i] );
    }    
    if(list.size() > 0)
    {
      items = new String[list.size()];
      for(int j = 0; j<list.size(); j++)
      {
        items[j] = ((FeatureTypeProperty)list.get(j)).getName();        
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
package org.kalypso.editor.tableeditor.layerTable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;

/**
 * @author bce
 */
public class LayerTableModel
{
  private final Collection m_listeners = new ArrayList();
  
  private final KalypsoFeatureLayer myLayer;

  private final Map m_ftpColumnMap = new LinkedHashMap();

  public LayerTableModel( final KalypsoFeatureLayer layer, final Column[] columns )
  {
    myLayer = layer;
    
    for( int i = 0; i < columns.length; i++ )
      m_ftpColumnMap.put( columns[i].ftp, columns[i] );
  }

  public KalypsoFeatureLayer getLayer()
  {
    return myLayer;
  }
  
  public boolean isColumn( final FeatureTypeProperty ftp )
  {
    return ( m_ftpColumnMap.get( ftp ) != null );
  }
  
  public void showColumn( final FeatureTypeProperty ftp, final boolean bShow ) throws LayerModelException
  {
    if( bShow )
    {
      if( isColumn(ftp))
        throw new LayerModelException( "Column exists already: " + ftp.getName() );
      
      m_ftpColumnMap.put( ftp, new Column( ftp, 100, false ) );
    }
    else
    {
      if( !isColumn(ftp) )
        throw new LayerModelException( "Column doesnt exist: " + ftp.getName() );
      
      m_ftpColumnMap.remove( ftp );
    }

    fireColumnsChanged();
  }
  
  public boolean isEditable( final FeatureTypeProperty ftp )
  {
    final Column column = (Column)m_ftpColumnMap.get( ftp );
    if( column != null )
      return column.isEditable;
    
    return false;
  }
  
  public int getInitialWidth( final FeatureTypeProperty ftp )
  {
    final Column column = (Column)m_ftpColumnMap.get( ftp );
    if( column != null )
      return column.initialWidth;
    
    return 0;
  }
  
  public void addModelListener( final ILayerTableModelListener l )
  {
    m_listeners.add( l );
  }

  public void removeModelListener( final ILayerTableModelListener l )
  {
    m_listeners.remove( l );
  }
  
  public void fireRowsChanged( final KalypsoFeature fe  )
  {
    for( Iterator iter = m_listeners.iterator(); iter.hasNext(); )
      ((ILayerTableModelListener)iter.next()).onRowsChanged( fe );
  }

  public void fireColumnsChanged(   )
  {
    for( Iterator iter = m_listeners.iterator(); iter.hasNext(); )
      ((ILayerTableModelListener)iter.next()).onColumnsChanged( );
  }

  public void addRow( final KalypsoFeature feature) throws Exception
  {
      myLayer.addFeature( feature );
      
      fireRowsChanged( feature );
  }

  public FeatureType getFeatureType()
  {
    return myLayer.getFeatureType();
  }


  public void removeRow( final KalypsoFeature feature ) throws Exception
  {
    myLayer.removeFeature( feature );

    fireRowsChanged( feature );
  }

  public final static class Column
  {
    public final boolean isEditable;
    public final FeatureTypeProperty ftp;
    public final int initialWidth;
    
    public Column( final FeatureTypeProperty ftpArg, final int intialWidthArg, final boolean isEditableArg )
    {
      ftp = ftpArg;
      isEditable = isEditableArg;
      initialWidth = intialWidthArg;
    }
  }
  
  public Column[] getColumns()
  {
    return (Column[])m_ftpColumnMap.values().toArray( new Column[m_ftpColumnMap.size()] );
  }
}
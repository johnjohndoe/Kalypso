package org.kalypso.editor.tableeditor.layerTable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.sort.DisplayContext;

/**
 * @author bce
 */
public class LayerTableModel
{
  private final Collection m_listeners = new ArrayList();
  
  private final KalypsoFeatureLayer myLayer;

  private final Map m_ftpColumnMap = new HashMap();

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
  
  public void fireRowsChanged( final DisplayContext dc  )
  {
    for( Iterator iter = m_listeners.iterator(); iter.hasNext(); )
      ((ILayerTableModelListener)iter.next()).onRowsChanged( dc );
  }

  public void fireColumnsChanged(   )
  {
    for( Iterator iter = m_listeners.iterator(); iter.hasNext(); )
      ((ILayerTableModelListener)iter.next()).onColumnsChanged( );
  }

  public void addRow( final DisplayContext dc) throws Exception
  {
      myLayer.add( dc );
      
      fireRowsChanged( dc );
  }


  public DisplayContext addRow( final Feature fe) throws Exception
  {
    final DisplayContext dc=myLayer.createDisplayContext( fe );  
    addRow(dc);
    return dc;
  }

  public FeatureType getFeatureType()
  {
    return myLayer.getFeatureType();
  }


  public void removeRow( final DisplayContext dc ) throws Exception
  {
    myLayer.remove( dc );

    fireRowsChanged( dc );
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
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

/**
 * @author bce
 */
public class LayerTableModel
{
  private final Collection m_listeners = new ArrayList();
  
  private final KalypsoFeatureLayer myLayer;

  private final Map m_layerVisible = new HashMap();

  public LayerTableModel( final KalypsoFeatureLayer layer, final String[] columns )
  {
    myLayer = layer;

    final FeatureTypeProperty[] ftps = layer.getFeatureType().getProperties();
    for( int i = 0; i < ftps.length; i++ )
    {
      boolean bVisible = false;
      // is visible?
      for( int j = 0; j < columns.length; j++ )
      {
        if( ftps[i].getName().equals( columns[j] ) )
        {
          bVisible = true;
          break;
        }
      }

      m_layerVisible.put( ftps[i], Boolean.valueOf( bVisible ) );
    }
  }

  public KalypsoFeatureLayer getLayer()
  {
    return myLayer;
  }

  public boolean isVisible( final FeatureTypeProperty ftp )
  {
    final Boolean visible = (Boolean)m_layerVisible.get( ftp );
    if( visible == null )
      throw new IllegalArgumentException( "Column not found: " + ftp );

    return visible.booleanValue();
  }

  public void setVisible( final FeatureTypeProperty ftp, final boolean bVisible )
  {
    m_layerVisible.put( ftp, Boolean.valueOf( bVisible ) );
    
    fireColumnChanged( ftp );
  }
  
  public void addModelListener( final ILayerTableModelListener l )
  {
    m_listeners.add( l );
  }

  public void removeModelListener( final ILayerTableModelListener l )
  {
    m_listeners.remove( l );
  }
  
  public void fireColumnChanged( final FeatureTypeProperty ftp )
  {
    for( Iterator iter = m_listeners.iterator(); iter.hasNext(); )
      ((ILayerTableModelListener)iter.next()).onColumnChanged(ftp);
  }
  
  public void fireRowsChanged( final Feature feature  )
  {
    for( Iterator iter = m_listeners.iterator(); iter.hasNext(); )
      ((ILayerTableModelListener)iter.next()).onRowsChanged( feature );
  }


  public FeatureTypeProperty[] getVisibleProperties()
  {
    final Collection props = new ArrayList();

    for( Iterator iter = m_layerVisible.entrySet().iterator(); iter.hasNext(); )
    {
      final Map.Entry entry = (Map.Entry)iter.next();
      if( ((Boolean)entry.getValue()).booleanValue() )
        props.add( entry.getKey() );
    }
    
    return (FeatureTypeProperty[])props.toArray( new FeatureTypeProperty[props.size()] );
  }


  public void addFeature( final Feature feature ) throws Exception
  {
      myLayer.addFeature( feature );
      
      fireRowsChanged( feature );
  }

  public FeatureType getFeatureType()
  {
    return myLayer.getFeatureType();
  }

  public void removeFeature( final Feature feature ) throws Exception
  {
    myLayer.removeFeature( feature );

    fireRowsChanged( feature );
  }
}
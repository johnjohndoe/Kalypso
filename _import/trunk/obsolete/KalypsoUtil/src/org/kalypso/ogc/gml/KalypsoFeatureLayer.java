package org.kalypso.ogc.gml;

import java.util.ArrayList;
import java.util.List;

import org.deegree.graphics.FeatureLayer;
import org.deegree.graphics.LayerEventController;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.event.ModellEventProvider;
import org.kalypso.ogc.event.ModellEventProviderAdapter;
import org.kalypso.ogc.sort.DisplayContext;
import org.kalypso.ogc.sort.DisplayContextSort;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author vdoemming
 */
public class KalypsoFeatureLayer implements FeatureLayer, ModellEventProvider, ModellEventListener
{
	
  private ModellEventProviderAdapter myEventProvider = new ModellEventProviderAdapter();
  
  // TODO hier kann die performance stark verbessert werden, wenn es einen
  // iterator ?ber den index der displaycontexte gaebe
  private String myName = null;

  private DisplayContextSort mySort = null;

  private FeatureType myFeatureType = null;

  public KalypsoFeatureLayer( final String name, final FeatureType featureType,
      final CS_CoordinateSystem crs )
  {
    myName = name;
    myFeatureType = featureType;
    mySort = new DisplayContextSort( crs );
    mySort.addModellListener(this);
  }
  
  public DisplayContextSort getSort()
  {
    return mySort;
  }

  /**
   * @see org.deegree.graphics.FeatureLayer#addFeature(org.deegree.model.feature.Feature)
   */
  public void addFeature( Feature feature ) throws Exception
  {
    mySort.add( feature );
    
    fireModellEvent(null);
  }
  
  public DisplayContext createDisplayContext(Feature fe)
  {
      return mySort.createDisplayContext(fe);
  }
  public void add(final DisplayContext dc)
  {
      mySort.add(dc);

      fireModellEvent(null);
    }

  /**
   * @see org.deegree.graphics.FeatureLayer#addFeatureCollection(org.deegree.model.feature.FeatureCollection)
   */
  public void addFeatureCollection( FeatureCollection featureCollection ) throws Exception
  {
    final Feature fe[] = featureCollection.getAllFeatures();
    for( int i = 0; i < fe.length; i++ )
      mySort.add( fe[i] );
    
    fireModellEvent(null);
  }

  /**
   * @see org.deegree.graphics.FeatureLayer#getAllFeatures()
   */
  public Feature[] getAllFeatures()
  {
    final List result = mySort.getAllFeatures();
    return (Feature[])result.toArray( new Feature[result.size()] );
  }

  public DisplayContext[] getAllDisplayContexts()
  {
    final List result = new ArrayList();
    mySort.queryAll(result);
    return (DisplayContext[])result.toArray( new DisplayContext[result.size()] );    
  }
  /**
   * @see org.deegree.graphics.FeatureLayer#getFeature(int)
   */
  public Feature getFeature( int index )
  {
    return (Feature)mySort.getAllFeatures().get( index );
  }

  /**
   * @see org.deegree.graphics.FeatureLayer#getFeatureById(java.lang.String)
   */
  public Feature getFeatureById( String id )
  {
    List list = mySort.getAllFeatures();
    for( int i = 0; i < list.size(); i++ )
    {
      if( id.equals( ( (DisplayContext)list.get( i ) ).getFeature().getId() ) )
        return ( (DisplayContext)list.get( i ) ).getFeature();
    }
    return null;
  }

  /**
   * @see org.deegree.graphics.FeatureLayer#getFeaturesById(java.lang.String[])
   */
  public Feature[] getFeaturesById( String[] id )
  {
    List list = mySort.getAllFeatures();
    List result = new ArrayList();
    for( int i = 0; i < list.size(); i++ )
    {
      if( id.equals( ( (DisplayContext)list.get( i ) ).getFeature().getId() ) )
        result.add( ( (DisplayContext)list.get( i ) ).getFeature() );
    }
    return (Feature[])result.toArray( new Feature[result.size()] );
  }

  public FeatureType getFeatureType()
  {
    return myFeatureType;
  }

  /**
   * @see org.deegree.graphics.FeatureLayer#getSize()
   */
  public int getSize()
  {
    return mySort.getAllFeatures().size();
  }

  /**
   * @see org.deegree.graphics.FeatureLayer#removeFeature(org.deegree.model.feature.Feature)
   */
  public void removeFeature( Feature feature ) throws Exception
  {
    mySort.remove( feature );
    
    fireModellEvent(null);
  }

  public void remove( DisplayContext dc) throws Exception
  {
    mySort.remove( dc );
    
    fireModellEvent(null);
  }
 /**
   * @see org.deegree.graphics.FeatureLayer#removeFeature(int)
   */
  public void removeFeature( int index ) throws Exception
  {
    final List list = mySort.getAllFeatures();
    mySort.remove( (Feature)list.get( index ) );
    
    fireModellEvent(null);
  }

  /**
   * @see org.deegree.graphics.FeatureLayer#removeFeatureCollection(org.deegree.model.feature.FeatureCollection)
   */
  public void removeFeatureCollection( FeatureCollection featureCollection ) throws Exception
  {
    Feature[] features = featureCollection.getAllFeatures();
    for( int i = 0; i < features.length; i++ )
      mySort.remove( features[i] );
    
    fireModellEvent(null);
  }

  /**
   * @see org.deegree.graphics.Layer#addEventController(org.deegree.graphics.LayerEventController)
   */
  public void addEventController( LayerEventController obj )
  {
  // not implemented, brauchen wir nicht
  }

  /**
   * @see org.deegree.graphics.Layer#getBoundingBox()
   */
  public GM_Envelope getBoundingBox()
  {
    return mySort.getBoundingBox();
  }

  /**
   * @see org.deegree.graphics.Layer#getCoordinatesSystem()
   */
  public CS_CoordinateSystem getCoordinatesSystem()
  {
    return mySort.getCoordinatesSystem();
  }

  /**
   * @see org.deegree.graphics.Layer#getName()
   */
  public String getName()
  {
    return myName;
  }

  /**
   * @see org.deegree.graphics.Layer#removeEventController(org.deegree.graphics.LayerEventController)
   */
  public void removeEventController( LayerEventController obj )
  {
  // not implemented, brauchen wir nicht
  }

  /**
   * @see org.deegree.graphics.Layer#setCoordinatesSystem(org.opengis.cs.CS_CoordinateSystem)
   */
  public void setCoordinatesSystem( CS_CoordinateSystem crs ) throws Exception
  {
    mySort.setCoordinatesSystem(crs);
    
    fireModellEvent(null);
  }
  
  public void optimize()
  {
    final Feature[] allFE = getAllFeatures();
    GM_Envelope env = getBoundingbox( allFE );
    final DisplayContextSort newSort = new DisplayContextSort( mySort.getCoordinatesSystem(), env );
    for( int i = 0; i < allFE.length; i++ )
    {
      try
      {
        newSort.add( allFE[i] );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
    mySort.removeModellListener(this);
    mySort = newSort;
    mySort.addModellListener(this);        
    fireModellEvent(null);
  }

  private GM_Envelope getBoundingbox( Feature[] fe )
  {
    double minx = 9E99;
    double maxx = -9E99;
    double miny = 9E99;
    double maxy = -9E99;

    for( int i = 0; i < fe.length; i++ )
    {
      Object[] prop = fe[i].getProperties();
      for( int k = 0; k < prop.length; k++ )
      {
        if( prop[k] instanceof GM_Object )
        {
          if( prop[k] instanceof GM_Point )
          {
            GM_Position pos = ( (GM_Point)prop[k] ).getPosition();
            if( pos.getX() > maxx )
            {
              maxx = pos.getX();
            }
            else if( pos.getX() < minx )
            {
              minx = pos.getX();
            }
            if( pos.getY() > maxy )
            {
              maxy = pos.getY();
            }
            else if( pos.getY() < miny )
            {
              miny = pos.getY();
            }
          }
          else
          {
            GM_Envelope en = ( (GM_Object)prop[k] ).getEnvelope();
            if( en.getMax().getX() > maxx )
            {
              maxx = en.getMax().getX();
            }
            if( en.getMin().getX() < minx )
            {
              minx = en.getMin().getX();
            }
            if( en.getMax().getY() > maxy )
            {
              maxy = en.getMax().getY();
            }
            if( en.getMin().getY() < miny )
            {
              miny = en.getMin().getY();
            }
          }
        }
      }
    }

    return GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );
  }
  
  public void addModellListener( ModellEventListener listener )
  {
    myEventProvider.addModellListener( listener );
  }
  public void fireModellEvent( ModellEvent event )
  {
    myEventProvider.fireModellEvent( event );
  }
  public void removeModellListener( ModellEventListener listener )
  {
    myEventProvider.removeModellListener( listener );
  }

/* (non-Javadoc)
 * @see org.kalypso.ogc.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
 */
public void onModellChange(ModellEvent modellEvent) {

fireModellEvent(modellEvent);
	
}

}
package org.kalypso.ogc.gml;

import java.util.List;

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
import org.kalypso.ogc.sort.KalypsoFeatureSort;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author vdoemming
 */
public class KalypsoFeatureLayer implements ModellEventProvider, ModellEventListener //, FeatureLayer
{
  private ModellEventProviderAdapter myEventProvider = new ModellEventProviderAdapter();

  private String myName = null;

  private KalypsoFeatureSort mySort = null;

  private FeatureType myFeatureType = null;

  public KalypsoFeatureLayer( final String name, final FeatureType featureType,
      final CS_CoordinateSystem crs )
  {
    myName = name;
    myFeatureType = featureType;
    mySort = new KalypsoFeatureSort( crs );
    mySort.addModellListener( this );
  }

  public KalypsoFeatureSort getSort()
  {
    return mySort;
  }

 
  public void addFeature( KalypsoFeature feature ) throws Exception
  {
    mySort.add( feature );
    fireModellEvent( null );
  }

  public void modifiedFeature(KalypsoFeature feature)
  {
   mySort.modifiedFeature(feature);
  }
  
  public void modifiedFeatures(KalypsoFeature[] features)
  {
   mySort.modifiedFeatures(features);
  }
  
  public void addFeatures( KalypsoFeature[] features ) throws Exception
  {
    for( int i = 0; i < features.length; i++ )
      mySort.add( features[i] );

    fireModellEvent( null );
  }

  public KalypsoFeature[] getAllFeatures()
  {
    final List result = mySort.getAllFeatures();
    return (KalypsoFeature[])result.toArray( new KalypsoFeature[result.size()] );
  }

  public KalypsoFeature getFeature( int index )
  {
    return (KalypsoFeature)mySort.getAllFeatures().get( index );
  }

  public KalypsoFeature getFeatureById( String id )
  {
    List list = mySort.getAllFeatures();
    for( int i = 0; i < list.size(); i++ )
    {
      if( id.equals( ( (KalypsoFeature)list.get( i ) ).getId() ) )
        return ( (KalypsoFeature)list.get( i ) );
    }
    return null;
  }

  public FeatureType getFeatureType()
  {
    return myFeatureType;
  }

  public int getSize()
  {
    return mySort.getAllFeatures().size();
  }

  public void removeFeature( KalypsoFeature feature ) throws Exception
  {
    mySort.remove( feature );
    fireModellEvent( null );
  }

  /**
   * @see org.deegree.graphics.FeatureLayer#removeFeature(int)
   */
  public void removeFeature( int index ) throws Exception
  {
    final List list = mySort.getAllFeatures();
    mySort.remove( (KalypsoFeature)list.get( index ) );
    fireModellEvent( null );
  }

  public void removeFeatures( KalypsoFeature[] features ) throws Exception
  {
    for( int i = 0; i < features.length; i++ )
      mySort.remove( features[i] );
    fireModellEvent( null );
  }

  public GM_Envelope getBoundingBox()
  {
    return mySort.getBoundingBox();
  }

  public CS_CoordinateSystem getCoordinatesSystem()
  {
    return mySort.getCoordinatesSystem();
  }

  public String getName()
  {
    return myName;
  }

  public void setCoordinatesSystem( CS_CoordinateSystem crs )
  {
    mySort.setCoordinatesSystem( crs );
    fireModellEvent( null );
  }

  public void optimize()
  {
    final KalypsoFeature[] allFE = getAllFeatures();
    GM_Envelope env = getBoundingbox( allFE );
    final KalypsoFeatureSort newSort = new KalypsoFeatureSort( mySort.getCoordinatesSystem(), env );
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
    mySort.removeModellListener( this );
    mySort = newSort;
    mySort.addModellListener( this );
    fireModellEvent( null );
  }

  private GM_Envelope getBoundingbox( KalypsoFeature[] fe )
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

  public void addModellListener( final ModellEventListener listener )
  {
    myEventProvider.addModellListener( listener );
  }

  public void fireModellEvent( final ModellEvent event )
  {
    myEventProvider.fireModellEvent( event );
  }

  public void removeModellListener( ModellEventListener listener )
  {
    myEventProvider.removeModellListener( listener );
  }

  /**
   * @see org.kalypso.ogc.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    fireModellEvent( modellEvent );
  }

}
package org.kalypso.ogc.gml;

import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree.model.feature.event.ModellEventProviderAdapter;
import org.deegree.model.geometry.GM_Envelope;
import org.kalypso.ogc.gml.sort.KalypsoFeatureSort;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author vdoemming
 */
public class KalypsoFeatureLayer implements IKalypsoLayer
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

 
  public void addFeature( Feature feature ) throws Exception
  {
    mySort.add( feature );
    fireModellEvent( null );
  }

  public void modifiedFeature(Feature feature)
  {
   mySort.modifiedFeature(feature);
  }
  
  public void modifiedFeatures(Feature[] features)
  {
   mySort.modifiedFeatures(features);
  }
  
  public void addFeatures( Feature[] features ) throws Exception
  {
    for( int i = 0; i < features.length; i++ )
      mySort.add( features[i] );

    fireModellEvent( null );
  }

  public Feature[] getAllFeatures()
  {
    final List result = mySort.getAllFeatures();
    return (Feature[])result.toArray( new Feature[result.size()] );
  }

  public Feature getFeature( int index )
  {
    return (Feature)mySort.getAllFeatures().get( index );
  }

  public Feature getFeatureById( String id )
  {
    List list = mySort.getAllFeatures();
    for( int i = 0; i < list.size(); i++ )
    {
      if( id.equals( ( (Feature)list.get( i ) ).getId() ) )
        return ( (Feature)list.get( i ) );
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

  public void removeFeature( Feature feature ) throws Exception
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
    mySort.remove( (Feature)list.get( index ) );
    fireModellEvent( null );
  }

  public void removeFeatures( Feature[] features ) throws Exception
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
    final Feature[] allFE = getAllFeatures();
    final KalypsoFeatureSort newSort = new KalypsoFeatureSort( mySort.getCoordinatesSystem() );
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
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    fireModellEvent( modellEvent );
  }

}
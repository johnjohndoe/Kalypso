package org.kalypso.ogc.gml.sort;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.List;

import org.deegree.graphics.displayelements.DisplayElement;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.sort.JMSpatialIndex;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.ogc.gml.KalypsoFeature;

public class SplitSort implements JMSpatialIndex
{
  public static boolean showIndexEnv = false;

  private SplitSortContainer myRootContainer = null;

  private List myObjects = new ArrayList(); // objects without
                                                           // geometry

  public SplitSort()
  {
    myRootContainer = null;
  }

  public void add( Object object )
  {
    GM_Envelope env = getEnvelope( object );
    if( env != null )
      add( env, object );
    else
      myObjects.add( object );
  }

  public void add( GM_Position pos, Object object )
  {
    add( GeometryFactory.createGM_Envelope( pos, pos ), object );
  }

  public void add( GM_Envelope env, Object object )
  {
    if( env == null )
    {
      return;
    }
    if(myRootContainer==null)
      myRootContainer=new SplitSortContainer(null,env);
    
    if( myRootContainer.getEnvelope().contains( env ) )
      myRootContainer.add( env, object );
    else
    {
      double maxX = env.getMax().getX();
      double maxY = env.getMax().getY();
      double minX = env.getMin().getX();
      double minY = env.getMin().getY();

      GM_Envelope envRoot = myRootContainer.getEnvelope();
      
      double maxXroot = envRoot.getMax().getX();
      double maxYroot = envRoot.getMax().getY();
      double minXroot = envRoot.getMin().getX();
      double minYroot = envRoot.getMin().getY();
      GM_Envelope newEnv = GeometryFactory.createGM_Envelope( minX < minXroot ? minX : minXroot,
          minY < minYroot ? minY : minYroot, maxX > maxXroot ? maxX : maxXroot,
          maxY > maxYroot ? maxY : maxYroot );

  
      SplitSortContainer newRootContainer = new SplitSortContainer( null, newEnv );
      myRootContainer.setParent( newRootContainer );
      newRootContainer.createSubContainers( myRootContainer );
      myRootContainer = newRootContainer;
      myRootContainer.add( env, object );
    }
  }

  public List query( GM_Envelope queryEnv, List result )
  {
    if( myRootContainer != null )
      myRootContainer.query( queryEnv, result );
    return result;
  }

  public List query( GM_Position pos, List result )
  {
    return query( GeometryFactory.createGM_Envelope( pos, pos ), result );
  }

  public List queryAll( List result )
  {
    result.addAll( myObjects );
    if( myRootContainer != null )
      myRootContainer.queryAll( result );
    return result;
  }

  public void remove( GM_Envelope env, Object object )
  {
    if( myRootContainer != null )
    {
      if( env != null )
        myRootContainer.remove( env, object );
      else
        myRootContainer.remove( object );
    }
  }

  public void remove( Object object )
  {
    GM_Envelope env = getEnvelope( object );
    if( env != null )
      remove( env, object );
    else
      myObjects.remove( object );
  }

 
  protected static GM_Envelope getEnvelope( Object object )
  {
    if( object instanceof DisplayContext )
    {
      return getEnvelope( ( (DisplayContext)object ).getFeature() );
    }
    if( object instanceof DisplayElement )
    {
      DisplayElement de = (DisplayElement)object;
      return getEnvelope( de.getFeature() );
    }
    else if( object instanceof KalypsoFeature )
    {
      KalypsoFeature fe = (KalypsoFeature)object;
      GM_Object gmObject = fe.getDefaultGeometryProperty();
      if( gmObject == null )
        return null;
      GM_Envelope env = gmObject.getEnvelope();
      if( env == null )
      {
        GM_Position gmPos = fe.getDefaultGeometryProperty().getCentroid().getPosition();
        env = GeometryFactory.createGM_Envelope( gmPos, gmPos );
      }
      return env;
    }
    else
    {
      return null;
    }
  }

  public void paint( Graphics g, GeoTransform geoTransform )
  {
    if( myRootContainer != null )
      myRootContainer.paint( g, geoTransform );
  }

  public int rsize()
  {
    if( myRootContainer != null )
      return myRootContainer.rsize();    
    return 0;
  }

  public GM_Envelope getBoundingBox()
  {
    // TODO es muss die boundingbox aus den Objecten innerhalb des
    // rootcontainers gebildet werden
    if( myRootContainer != null )
      return myRootContainer.getEnvelope();
    return null;
  }

  /**
   * @see org.deegree.model.sort.JMSpatialIndex#resort(org.deegree.model.geometry.GM_Envelope, org.deegree.model.geometry.GM_Envelope, java.lang.Object)
   */
  public void resort( GM_Envelope newEnv, GM_Envelope oldEnv, Object object )
  {
    // nicht implementiert  
  }

  /**
   * @see org.deegree.model.sort.JMSpatialIndex#resort(org.deegree.model.geometry.GM_Envelope, java.lang.Object)
   */
  public void resort( GM_Envelope newEnv, Object object )
  {
    // nicht implementiert
  }
}
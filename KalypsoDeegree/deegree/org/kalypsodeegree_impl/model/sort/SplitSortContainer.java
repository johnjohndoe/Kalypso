package org.deegree_impl.model.sort;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.List;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.geometry.GeometryFactory;

public class SplitSortContainer
{
  private static final int MAX_OBJECTS = 100;

  private SplitSortContainer[] mySubContainer =
  { null, null, null, null };

  private GM_Envelope myEnvelope;

  private List myObjects = new ArrayList();

  private static final int LEFT_BOTTOM = 0;

  private static final int RIGHT_BOTTOM = 1;

  private static final int RIGHT_TOP = 2;

  private static final int LEFT_TOP = 3;

  public GM_Envelope getEnvelope()
  {
    return myEnvelope;
  }

  public void setParent( SplitSortContainer container )
  {
    myParent = container;
  }

  public int size()
  {
    return myObjects.size();
  }

  public int rsize()
  {
    int size = size();
    for( int i = 0; i < 4; i++ )
      if( mySubContainer[i] != null )
        size += mySubContainer[i].rsize();
    return size;
  }

  /*
   * private int getFittingSubContainer(GM_Envelope env) {
   * if(hasSubContainers()) { if() } }
   */
  private SplitSortContainer myParent = null;

  public SplitSortContainer( SplitSortContainer parent, GM_Envelope env )
  {
    myEnvelope = env;
    myParent = parent;
    try
    {
      //		add(org.deegree_impl.graphics.displayelements.DisplayElementFactory.buildDisplayElement(env));
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  private void add( Object object )
  {
    //	System.out.println("splitsortcontaier.add...");
    myObjects.add( object );
  }

  private void resort()
  {
    List oldObjects = myObjects;
    myObjects = new ArrayList();
    for( int i = 0; i < oldObjects.size(); i++ )
    {
      GM_Envelope env = SplitSort.getEnvelope( oldObjects.get( i ) );
      if( env != null )
        add( env, oldObjects.get( i ) );
      else
        add( oldObjects.get( i ) );
    }
  }

  public void add( GM_Envelope env, Object object )
  {
    if( !myEnvelope.contains( env ) )
      {
//    Debug.println("ERROR: Container.add() does not contain envelope :-(");
      }
    else if( !hasSubContainers() && myObjects.size() < MAX_OBJECTS )
    {
      add( object );
    }
    /*
     * else if(!hasSubContainers() && myObjects.size() <MAX_OBJECTS) {
     * createSubContainers(); add(env,object); }
     */
    else if( hasSubContainers() )
    {
      if( mySubContainer[0].getEnvelope().contains( env ) )
        mySubContainer[0].add( env, object );
      else if( mySubContainer[1].getEnvelope().contains( env ) )
        mySubContainer[1].add( env, object );
      else if( mySubContainer[2].getEnvelope().contains( env ) )
        mySubContainer[2].add( env, object );
      else if( mySubContainer[3].getEnvelope().contains( env ) )
        mySubContainer[3].add( env, object );
      else
      {
        add( object );
      }
    }
    else
    // has no subContainer && myObjects.size() >=MAX_OBEJCTS
    {
      createSubContainers();
      //		if(size()>RESORT_SIZE)
      resort();
      add( env, object );
    }
  }

  /*
   * public boolean contains(GM_Envelope env) { return myEnvelope.contains(env); }
   */
  public boolean intersects( GM_Envelope env )
  {
    return myEnvelope.intersects( env );
  }

  public void createSubContainers( SplitSortContainer container )
  {

    double midX = 0d;
    double midY = 0d;
    boolean midXset = false;
    boolean midYset = false;
    GM_Envelope subEnv = container.getEnvelope();

    double maxX = myEnvelope.getMax().getX();
    double maxY = myEnvelope.getMax().getY();
    double minX = myEnvelope.getMin().getX();
    double minY = myEnvelope.getMin().getY();

    double maxXsub = subEnv.getMax().getX();
    double maxYsub = subEnv.getMax().getY();
    double minXsub = subEnv.getMin().getX();
    double minYsub = subEnv.getMin().getY();

    if( maxX == maxXsub )
    {
      midX = minXsub;
      midXset = true;
    }
    if( minX == minXsub )
    {
      midX = maxXsub;
      midXset = true;
    }
    if( maxY == maxYsub )
    {
      midY = minYsub;
      midYset = true;
    }
    if( minY == minYsub )
    {
      midY = maxYsub;
      midYset = true;
    }
    if( midXset && midYset )
    {
      createSubContainers( midX, midY );
      for( int i = 0; i < 4; i++ )
      {
        if( mySubContainer[i].getEnvelope().equals( container.getEnvelope() ) )
        {
          mySubContainer[i] = container;
        }
      }
    }
    else
    {
      midX = ( minX + maxX ) / 2d;
      midY = ( minY + maxY ) / 2d;
      switch( bestPoint( midX, midY, minXsub, minYsub, maxXsub, maxYsub ) )
      {
      case LEFT_BOTTOM:
        createSubContainers( minXsub, minYsub );
        break;
      case RIGHT_BOTTOM:
        createSubContainers( maxXsub, minYsub );
        break;
      case RIGHT_TOP:
        createSubContainers( maxXsub, maxYsub );
        break;
      case LEFT_TOP:
        createSubContainers( minXsub, maxYsub );
        break;
      }
      for( int i = 0; i < 4; i++ )
      {
        if( mySubContainer[i].getEnvelope().contains( container.getEnvelope() ) )
        {
          mySubContainer[i].createSubContainers( container );
        }
      }
    }
  }

  private int bestPoint( double midX, double midY, double minX, double minY, double maxX,
      double maxY )
  {
    double dist[] = new double[4];
    dist[LEFT_BOTTOM] = Math.pow( minX - midX, 2d ) + Math.pow( minY - midY, 2d );
    dist[RIGHT_BOTTOM] = Math.pow( maxX - midX, 2d ) + Math.pow( minY - midY, 2d );
    dist[RIGHT_TOP] = Math.pow( maxX - midX, 2d ) + Math.pow( maxY - midY, 2d );
    dist[LEFT_TOP] = Math.pow( minX - midX, 2d ) + Math.pow( maxY - midY, 2d );
    int result = 0;
    for( int i = 0; i < 4; i++ )
    {
      if( dist[i] < dist[result] )
        result = i;
    }
    return result;
  }

  private void createSubContainers()
  {
    double maxX = myEnvelope.getMax().getX();
    double maxY = myEnvelope.getMax().getY();

    double minX = myEnvelope.getMin().getX();
    double minY = myEnvelope.getMin().getY();

    double midX = ( minX + maxX ) / 2d;
    double midY = ( minY + maxY ) / 2d;
    createSubContainers( midX, midY );
  }

  private void createSubContainers( double midX, double midY )
  {
    double maxX = myEnvelope.getMax().getX();
    double maxY = myEnvelope.getMax().getY();

    double minX = myEnvelope.getMin().getX();
    double minY = myEnvelope.getMin().getY();

    GM_Envelope env[] =
    { GeometryFactory.createGM_Envelope( minX, minY, midX, midY ),
        GeometryFactory.createGM_Envelope( midX, minY, maxX, midY ),
        GeometryFactory.createGM_Envelope( midX, midY, maxX, maxY ),
        GeometryFactory.createGM_Envelope( minX, midY, midX, maxY ) };
    for( int i = 0; i < 4; i++ )
      mySubContainer[i] = new SplitSortContainer( this, env[i] );
  }

  private boolean hasSubContainers()
  {
    for( int i = 0; i < 4; i++ )
      if( mySubContainer[i] == null )
        return false;
    return true;
  }

  public void query( GM_Envelope env, List result )
  {
    if(result==null)
      result=new ArrayList();
    for( int i = 0; i < myObjects.size(); i++ )
    {
      GM_Envelope envObject = SplitSort.getEnvelope( myObjects.get( i ) );
      if( envObject == null || env.intersects( envObject ) )
        result.add( myObjects.get( i ) );
    }
    //	boolean queried=false;
    if( hasSubContainers() )
    {
      for( int i = 0; i < 4; i++ )
      {
        if( mySubContainer[i].getEnvelope().contains( env ) )
        {
          mySubContainer[i].query( env, result );
          return;
        }
      }
      for( int i = 0; i < 4; i++ )
      {
        if( env.intersects( mySubContainer[i].getEnvelope() ) )
          mySubContainer[i].query( env, result );
      }
    }
  }

  public void queryAll( List result )
  {
    if( hasSubContainers() )
    {
      for( int i = 0; i < 4; i++ )
        mySubContainer[i].queryAll( result );
    }
    result.addAll( myObjects );
  }

  public void remove( GM_Envelope env, Object object )
  {
    boolean removed = false;
    for( int i = 0; i < 4; i++ )
    {
      if( mySubContainer[i] != null && mySubContainer[i].getEnvelope().contains( env ) )
      {
        mySubContainer[i].remove( env, object );
        removed = true;
      }
    }
    for( int i = 0; i < 4; i++ )
    {
      if( mySubContainer[i] != null && mySubContainer[i].getEnvelope().intersects( env ) )
        mySubContainer[i].remove( env, object );
    }
    /*
     * env.contains(mySubContainer[i].getEnvelope()) //performance ||
     * mySubContainer[i].getEnvelope().contains(env) //performance ||
     * env.intersects(mySubContainer[i].getEnvelope())
     */
    if( !removed )
    {
      myObjects.remove( object );
      if( myParent != null )
        myParent.optimize();
    }
  }

  public void remove( Object object )
  {
    if( myObjects.contains( object ) )
    {
      myObjects.remove( object );
      if( myParent != null )
        myParent.optimize();
    }
    else if( hasSubContainers() )
      for( int i = 0; i < 4; i++ )
        mySubContainer[i].remove( object );

  }

  private void optimize()
  {
    if( hasSubContainers() )
    {
      for( int i = 0; i < 4; i++ )
      {
        if( mySubContainer[i].hasSubContainers() )
          return;
        if( mySubContainer[i].size() > 0 )
          return;
      }
      mySubContainer = new SplitSortContainer[]
      { null, null, null, null };
    }
    if( hasSubContainers() )
      return;
    if( size() > 0 )
      return;
    if( myParent != null )
      myParent.optimize();
  }

  public void paint( Graphics g, GeoTransform geoTransform )
  {
    double g1x = geoTransform.getDestX( myEnvelope.getMin().getX() );
    double g1y = geoTransform.getDestY( myEnvelope.getMin().getY() );
    double g2x = geoTransform.getDestX( myEnvelope.getMax().getX() );
    double g2y = geoTransform.getDestY( myEnvelope.getMax().getY() );

    g.drawRect( (int)( g1x < g2x ? g1x : g2x ), (int)( g1y < g2y ? g1y : g2y ), (int)Math
        .abs( ( g2x - g1x ) ), (int)Math.abs( ( g2y - g1y ) ) );

    if( hasSubContainers() )
      for( int i = 0; i < 4; i++ )
        mySubContainer[i].paint( g, geoTransform );
  }
}
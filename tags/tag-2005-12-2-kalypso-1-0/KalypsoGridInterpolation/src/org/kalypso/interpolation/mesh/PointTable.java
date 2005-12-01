/*
 * Created on 13.12.2004
 * 
 * TODO To change the template for this generated file go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation.mesh;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.sort.SplitSort;

/**
 * @author kuepfer
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class PointTable extends HashMap
{
  private String name = null;

  // TODO ?? how to implement the geo-search
  //private SplitSort geoList = null;
  public PointTable()
  {
  }

  public PointTable( String name )
  {
    this.name = name;
  }

  public void addPoint( Point p )
  {
    //System.out.println("* " + p.getPointID() + " *");
    put( p.getPointID(), p );
  }

  public String getTableName()
  {
    return name;
  }

  public int removePoint( Point p )
  {
    String id = p.getPointID();
    if( containsKey( id ) )
    {
      remove( id );
      return 1;
    }

    return -1;
  }//removePoint

  public Point getPoint( String id )
  {
    return (Point)get( id );
  }

  public Point getPoint( Point p )
  {
    return (Point)get( p.getPointID() );
  }

  public PointTable getSpatialSelection( GM_Envelope env )
  {
    PointTable selection = new PointTable();
    SplitSort sortedList = new SplitSort( null, null );
    Iterator itKeys = this.keySet().iterator();
    while( itKeys.hasNext() )
    {
      Point p = (Point)get( itKeys.next() );
      Feature pfeature = p.getFeature();
      sortedList.add( pfeature );
    }//while
    Vector list = new Vector();
    sortedList.query( env, list );
    if( list != null )
    {
      for( int i = 0; i < list.size(); i++ )
      {
        Feature f = (Feature)list.elementAt( i );
        selection.addPoint( getPoint( String.valueOf( f.getId() ) ) );
      }//for
    }//if
    return selection;
  }//getSpatialSelection

  public void addPoints( PointTable pt )
  {
    Iterator it = pt.keySet().iterator();
    while( it.hasNext() )
    {
      Point p = pt.getPoint( (String)it.next() );
      addPoint( p );
    }
  }//addPoints

  public PointTable getPoints( Element e )
  {
    PointTable pt = new PointTable();
    
    Vector points = e.getVertList();
    Iterator it = points.iterator();
    while( it.hasNext() )
    {
      Point p = getPoint( (String)it.next() );
      if( p != null )
        pt.addPoint( p );
    }
    return pt;
  }//getVerticiesFromElement

}//class

/*
 * Created on 13.12.2004
 * 
 * TODO To change the template for this generated file go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation.mesh;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;
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
public class ElementTable extends HashMap
{
  private String name = null;

  /*
   * TODO ?? how to implement the geo-search private SplitSort geoList = null;
   */
  public ElementTable()
  {
  }

  public ElementTable( String name )
  {
    this.name = name;
  }

  public void addElement( Element e )
  {
    put( e.getElementID(), e );
  }

  public String getTableName()
  {
    return name;
  }

  public int removeElement( Element e )
  {
    String id = e.getElementID();
    if( containsKey( id ) )
    {
      remove( id );
      return 1;
    }

    return -1;
  }//removePoint

  public Element getElement( String id )
  {
    return (Element)get( id );
  }

  public ElementTable getSpatialSelection( GM_Envelope env )
  {
    ElementTable selection = new ElementTable();
    SplitSort sortedList = new SplitSort( null, null );
    Iterator itKeys = this.keySet().iterator();
    while( itKeys.hasNext() )
    {
      Element e = (Element)get( itKeys.next() );
      Feature efeature = e.getFeature();
      sortedList.add( efeature );
    }//while
    Vector list = new Vector();
    sortedList.query( env, list );
    if( list != null )
    {
      for( int i = 0; i < list.size(); i++ )
      {
        Feature f = (Feature)list.elementAt( i );
        selection.addElement( getElement( String.valueOf( f.getId() ) ) );
      }//for
    }//if
    return selection;
  }//getSpatialSelection

  public TreeSet getNodes()
  {
    TreeSet ts = new TreeSet();
    Iterator keySet = keySet().iterator();
    while( keySet.hasNext() )
    {
      ts.addAll( getElement( (String)keySet.next() ).getVertList() );
    }
    return ts;
  }

  public GM_Envelope getFullBoundingBox()
  {

    Set set = keySet();
    GM_Envelope mergedEnv = null;
    int counter = 0;
    for( Iterator iter = set.iterator(); iter.hasNext(); )
    {
      Element e = (Element)get( iter.next() );
      if( counter == 0 )
        mergedEnv = e.getBBox();
      else
      {
        GM_Envelope env = e.getBBox();
        mergedEnv = mergedEnv.getMerged( env );
      }
      counter++;
    }
    return mergedEnv;
  }

}//class ElementTable

package org.kalypso.grid.processes.raster2vector;

import java.util.ArrayList;
import java.util.Collection;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * Element einer verketteten Liste von Cooridnaten
 * 
 * @author belger
 */
public class LinkedCoordinate
{
  public final Coordinate crd;

  public final boolean border;

  private LinkedCoordinate m_head = null;

  private LinkedCoordinate m_tail = null;

  public LinkedCoordinate( final Coordinate c, final boolean bBorder )
  {
    crd = c;
    border = bBorder;
  }

  public void link( final LinkedCoordinate list ) throws LinkedCoordinateException
  {
    // die andere Liste an ein freies ende hängen
    // eine der listen muss vieleicht umgedreht werden
    if( m_head == null && list.m_tail == null )
    {
      m_head = list;
      list.m_tail = this;
      return;
    }

    if( m_tail == null && list.m_head == null )
    {
      m_tail = list;
      list.m_head = this;
      return;
    }

    if( m_tail == null && list.m_tail == null )
    {
      reverseHead( list );
      m_tail = list;
      list.m_head = this;
      return;
    }

    if( m_head == null && list.m_head == null )
    {
      reverseTail( list );
      m_head = list;
      list.m_tail = this;
      return;
    }

    throw new LinkedCoordinateException( "Listen nicht linkbar" );
  }

  public boolean isCircle( )
  {
    LinkedCoordinate current = this;
    while( current.m_head != null )
    {
      if( current.m_head == this )
        return true;

      current = current.m_head;
    }

    return false;
  }

  private static void reverseHead( final LinkedCoordinate list ) throws LinkedCoordinateException
  {
    if( list.m_tail != null )
      throw new LinkedCoordinateException( "reverse does not start at beginning" );

    LinkedCoordinate current = list;
    while( current != null )
    {
      final LinkedCoordinate head = current.m_head;

      current.m_head = current.m_tail;
      current.m_tail = head;

      current = head;

      // damits auch bei Kreisen geht
      if( current == list )
        return;
    }
  }

  private static void reverseTail( final LinkedCoordinate list ) throws LinkedCoordinateException
  {
    if( list.m_head != null )
      throw new LinkedCoordinateException( "reverse does not start at beginning" );

    LinkedCoordinate current = list;
    while( current != null )
    {
      final LinkedCoordinate tail = current.m_tail;

      current.m_tail = current.m_head;
      current.m_head = tail;

      current = tail;

      // damits auch bei Kreisen geht
      if( current == list )
        return;
    }
  }

  public Coordinate[] getAsRing( ) throws LinkedCoordinateException
  {
    if( !isCircle() )
      throw new LinkedCoordinateException( "getLinearRing von offener Kurve" );

    final Collection<Coordinate> currentString = new ArrayList<Coordinate>();

    // Startplatz ändern
    LinkedCoordinate currentCrd = this;
    while( currentCrd != null )
    {
      currentString.add( currentCrd.crd );

      currentCrd = currentCrd.m_head;

      if( currentCrd == this )
      {
        currentString.add( currentCrd.crd );
        break;
      }
    }

    // return gf.createLinearRing( s_orientFilter.filter( crds ) );
    return currentString.toArray( new Coordinate[currentString.size()] );
  }

  public Collection<LineString> getLineStrings( final GeometryFactory gf )
  {
    final Collection<LineString> strings = new ArrayList<LineString>();

    Collection<Coordinate> currentString = new ArrayList<Coordinate>();

    // Startplatz ändern
    LinkedCoordinate start = this;
    while( start.border && start.m_head != this )
      start = start.m_head;

    LinkedCoordinate currentCrd = start;
    while( currentCrd != null )
    {
      if( currentCrd.border )
      {
        if( currentString.size() > 1 )
          strings.add( gf.createLineString( currentString.toArray( new Coordinate[currentString.size()] ) ) );

        currentString = new ArrayList<Coordinate>();
      }
      else
        currentString.add( currentCrd.crd );

      currentCrd = currentCrd.m_head;

      if( currentCrd == start )
      {
        currentString.add( currentCrd.crd );
        break;
      }
    }

    if( currentString.size() > 1 )
      strings.add( gf.createLineString( currentString.toArray( new Coordinate[currentString.size()] ) ) );

    return strings;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return "Crd: " + crd + " Border: " + border;
  }

  /**
   * Gibt true zurück, wenn alle Koodinaten am Rand liegen
   */
  public boolean onlyBorder( )
  {
    LinkedCoordinate current = this;
    while( current != null )
    {
      if( !current.border )
        return false;

      current = current.m_head;

      if( current == this )
        break;
    }

    return true;
  }

}

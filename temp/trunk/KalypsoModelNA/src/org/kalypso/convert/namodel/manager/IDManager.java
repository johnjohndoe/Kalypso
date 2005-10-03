/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.convert.namodel.manager;

import java.io.IOException;
import java.io.Writer;
import java.text.ParseException;
import java.util.Comparator;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.TreeSet;

import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypsodeegree.model.feature.Feature;

/**
 * 
 * @author doemming
 */
public class IDManager
{
  public final static int CHANNEL = 1;

  public final static int CATCHMENT = 2;

  public final static int NODE = 3;

  final Hashtable m_featureIDMap = new Hashtable(); // feature -> IDMap

  final Hashtable m_idMapFeature = new Hashtable(); // IDMap -> feature

  public IDManager()
  {
    m_idMapFeature.put( new IDMap( 10000, NODE ), new Object() );
    m_idMapFeature.put( new IDMap( 9001, NODE ), new Object() );
  }

  public Feature getFeature( int asciiID, int type )
  {
    final IDMap map = new IDMap( asciiID, type );
    if( m_idMapFeature.containsKey( map ) )
    {
      return (Feature)m_idMapFeature.get( map );
    }
    return null;
  }

  public int getAsciiID( Feature feature )
  {
    if( !m_featureIDMap.containsKey( feature ) )
    {
      final IDMap idMap = generateAsciiID( feature );
      m_featureIDMap.put( feature, idMap );
      m_idMapFeature.put( idMap, feature );
    }
    return ( (IDMap)m_featureIDMap.get( feature ) ).getAsciiID();
  }

  /**
   * @param feature
   */
  private int getType( Feature feature )
  {
    String name = feature.getFeatureType().getName();
    if( name.equals( "Catchment" ) )
      return CATCHMENT;
    if( name.endsWith( "Channel" ) )
      return CHANNEL;
    if( name.equals( "Node" ) )
      return NODE;
    throw new UnsupportedOperationException();
  }

  /**
   * 
   * @param feature
   */
  private IDMap generateAsciiID( Feature feature )
  {
    final String idProp = "name";
    int type = getType( feature );
    //    switch( type )
    //    {
    //    case CATCHMENT:
    //      idProp = "inum";
    //      break;
    //    case CHANNEL:
    //      idProp = "inum";
    //      break;
    //    case NODE:
    //      idProp = "num";
    //      break;
    //    default:
    //      idProp = "name";
    //    }
    final Object property = feature.getProperty( idProp );
    /**
     * reserviert sind die Knoten 9001 (Anfangsknoten) <br>
     * 10000 (Endknoten) >= 10001 (Str�nge, um das Netz zu vervollst�ndigen)
     */
    try
    {
      if( property != null )
      {
        int testID = NumberUtils.toInteger( property.toString() );
        //      int testID = Integer.parseInt( property.toString() );
        if( testID >= 1000 && testID < 10000 )
        {
          final IDMap map = new IDMap( testID, type );
          if( !m_idMapFeature.containsKey( map ) )
            return map;
        }
      }
    }
    catch( ParseException e )
    {
//      e.printStackTrace();
      // ignore exception and generate new id
    }
    int testID = 1000;
    IDMap testMap = new IDMap( testID, type );

    while( m_idMapFeature.containsKey( testMap ) )
    {
      testID++;
      testMap = new IDMap( testID, type );
    }
    return testMap;
  }

  public void dump( final Writer writer ) throws IOException
  {
    final TreeSet sort = new TreeSet( new Comparator()
    {
      public boolean equals( Object obj )
      {
        return false;
      }

      public int compare( Object o1, Object o2 )
      {
        IDMap m1 = (IDMap)o1;
        IDMap m2 = (IDMap)o2;
        int typeDiff = m1.getType() - m2.getType();
        if( typeDiff != 0 )
          return typeDiff;
        return m1.getAsciiID() - m2.getAsciiID();
      }
    } );
    sort.addAll( m_idMapFeature.keySet() );
    for( Iterator iter = sort.iterator(); iter.hasNext(); )
    {
      IDMap idmap = (IDMap)iter.next();
      writer.write( idmap.toString() );
      writer.write( "\t" );
      final Object value = m_idMapFeature.get( idmap );
      if( value instanceof Feature )
      {
        Feature feature = (Feature)value;
        writer.write( feature.getId() );
      }
      else
        writer.write( "dummy" );
      writer.write( "\n" );
    }
  }

  public class IDMap
  {
    final int type;

    int asciiID;

    public IDMap( int asciiID, int type )
    {
      this.type = type;
      this.asciiID = asciiID;
    }

    public int getAsciiID()
    {
      return asciiID;
    }

    public int getType()
    {
      return type;
    }

    public String toString()
    {
      return asciiID + "\t" + type;
    }

    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals( Object other )
    {
      if( !( other instanceof IDMap ) )
        return false;
      final IDMap otherMap = (IDMap)other;
      return otherMap.getAsciiID() == asciiID && otherMap.getType() == type;
    }

    /**
     * @see java.lang.Object#hashCode()
     */
    public int hashCode()
    {
      return asciiID + type * 100000;
    }
  }
}

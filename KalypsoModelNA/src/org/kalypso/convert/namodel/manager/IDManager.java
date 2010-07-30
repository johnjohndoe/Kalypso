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
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Hashtable;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.TreeSet;

import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author doemming
 */
public class IDManager
{
  public final static int CHANNEL = 1;

  public final static int CATCHMENT = 2;

  public final static int NODE = 3;

  final Hashtable<Object, IDMap> m_featureIDMap = new Hashtable<Object, IDMap>(); // feature -> IDMap

  final Hashtable<IDMap, Object> m_idMapFeature = new Hashtable<IDMap, Object>(); // IDMap -> feature

  /**
   * key: catchemntID<br>
   * value: list hydID in reihenfolge
   */
  private final Hashtable<String, List<String>> m_hydrohash = new Hashtable<String, List<String>>();

  public IDManager( )
  {
    m_idMapFeature.put( new IDMap( 10000, NODE ), new Object() );
    m_idMapFeature.put( new IDMap( 9001, NODE ), new Object() );
  }

  public Feature getFeature( final int asciiID, final int type )
  {
    final IDMap map = new IDMap( asciiID, type );
    if( m_idMapFeature.containsKey( map ) )
      return (Feature) m_idMapFeature.get( map );

    return null;
  }

  public int getAsciiID( final Feature feature )
  {
    if( !m_featureIDMap.containsKey( feature ) )
    {
      final IDMap idMap = generateAsciiID( feature );
      m_featureIDMap.put( feature, idMap );
      m_idMapFeature.put( idMap, feature );
    }
    return m_featureIDMap.get( feature ).getAsciiID();
  }

  /**
   * @param feature
   */
  private int getType( final Feature feature )
  {
    final String name = feature.getFeatureType().getQName().getLocalPart();
    if( name.equals( "Catchment" ) ) //$NON-NLS-1$
      return CATCHMENT;
    if( name.endsWith( "Channel" ) ) //$NON-NLS-1$
      return CHANNEL;
    if( name.equals( "Node" ) ) //$NON-NLS-1$
      return NODE;
    throw new UnsupportedOperationException();
  }

  /**
   * @param feature
   */
  private IDMap generateAsciiID( final Feature feature )
  {
    final int type = getType( feature );
    final String property = feature.getName();
    /**
     * reserviert sind die Knoten 9001 (Anfangsknoten) <br>
     * 10000 (Endknoten) >= 10001 (Stränge, um das Netz zu vervollständigen)
     */
    try
    {
      if( property != null )
      {
        final int testID = NumberUtils.toInteger( property );
        // int testID = Integer.parseInt( property.toString() );
        if( testID >= 1000 && testID < 10000 )
        {
          final IDMap map = new IDMap( testID, type );
          if( !m_idMapFeature.containsKey( map ) )
            return map;
        }
      }
    }
    catch( final ParseException e )
    {
      // e.printStackTrace();
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
    final TreeSet<IDMap> sort = new TreeSet<IDMap>( new Comparator<Object>()
        {
      @Override
      public boolean equals( final Object obj )
      {
        return false;
      }

      @Override
      public int compare( final Object o1, final Object o2 )
      {
        final IDMap m1 = (IDMap) o1;
        final IDMap m2 = (IDMap) o2;
        final int typeDiff = m1.getType() - m2.getType();
        if( typeDiff != 0 )
          return typeDiff;
        return m1.getAsciiID() - m2.getAsciiID();
      }
        } );
    sort.addAll( m_idMapFeature.keySet() );
    writer.write( String.format( Locale.US, "%-10s%-6s%-16s %-32s %-32s %-32s\n\n", "ASCII ID", "", "GML Type", "GML ID", "GML Name", "GML Description" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
    for( final IDMap idmap : sort )
    {
      writer.write( String.format( Locale.US, "%-10s -->  ", idmap.toString() ) ); //$NON-NLS-1$
      final Object value = m_idMapFeature.get( idmap );
      if( value instanceof Feature )
      {
        final Feature feature = (Feature) value;
        final String type = feature.getFeatureType().getQName().getLocalPart().toString();
        writer.write( String.format( Locale.US, "%-16s %-32s %-32s %-32s\n", type, feature.getId(), feature.getName(), feature.getDescription() ) ); //$NON-NLS-1$
      }
      else
      {
        writer.write( "[control entry]\n" ); //$NON-NLS-1$
      }
    }
  }

  public List<Feature> getAllFeaturesFromType( final int type )
  {
    final List<Feature> result = new ArrayList<Feature>();
    for( final Object featureObject : m_featureIDMap.keySet() )
    {
      if( featureObject instanceof Feature && getType( (Feature) featureObject ) == type )
        result.add( (Feature) featureObject );
    }
    return result;
  }

  public class IDMap
  {
    final int type;

    int asciiID;

    public IDMap( @SuppressWarnings("hiding") final int asciiID, @SuppressWarnings("hiding") final int type )
    {
      this.type = type;
      this.asciiID = asciiID;
    }

    public int getAsciiID( )
    {
      return asciiID;
    }

    public int getType( )
    {
      return type;
    }

    @Override
    public String toString( )
    {
      return asciiID + " [" + type + "]"; //$NON-NLS-1$ //$NON-NLS-2$
    }

    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object other )
    {
      if( !(other instanceof IDMap) )
        return false;
      final IDMap otherMap = (IDMap) other;
      return otherMap.getAsciiID() == asciiID && otherMap.getType() == type;
    }

    /**
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode( )
    {
      return asciiID + type * 100000;
    }
  }

  public void addHydroInfo( final Feature catchmentFE, final List<String> hydrIdList )
  {
    m_hydrohash.put( catchmentFE.getId(), hydrIdList );

  }

  public String getHydroFeatureId( final Feature catchmentFE, final int pos )
  {
    final List<String> hydIdList = m_hydrohash.get( catchmentFE.getId() );
    return hydIdList.get( pos );
  }

  public Set<String> getCatchmentIdsFromLzsim( )
  {
    return m_hydrohash.keySet();
  }

  public List<String> getSortedHydrosIDsfromLzsim( final String catchmentID )
  {
    return m_hydrohash.get( catchmentID );
  }

}

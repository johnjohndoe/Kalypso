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
package org.kalypso.model.hydrology.internal;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author doemming
 */
public class IDManager
{
  public final static int CHANNEL = 1;

  public final static int CATCHMENT = 2;

  public final static int NODE = 3;

  final Map<Feature, IDMap> m_featureIDMap = new HashMap<Feature, IDMap>(); // feature -> IDMap

  final Map<IDMap, Feature> m_idMapFeature = new HashMap<IDMap, Feature>(); // IDMap -> feature

  public IDManager( )
  {
    m_idMapFeature.put( new IDMap( 10000, NODE ), null );
    m_idMapFeature.put( new IDMap( 9001, NODE ), null );
  }

  public Feature getFeature( final int asciiID, final int type )
  {
    final IDMap map = new IDMap( asciiID, type );
    if( m_idMapFeature.containsKey( map ) )
      return m_idMapFeature.get( map );

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
      if( !StringUtils.isBlank( property ) )
      {
        final int testID = NumberUtils.toInteger( property );
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

  public void dump( File idMapFile ) throws SimulationException
  {
    Writer idWriter = null;
    try
    {
      idWriter = new FileWriter( idMapFile ); //$NON-NLS-1$
      dump( idWriter );
      idWriter.close();
    }
    catch( final IOException e )
    {
      throw new SimulationException( "Failed to dump idManager", e );
    }
    finally
    {
      IOUtils.closeQuietly( idWriter );
    }
  }
  
  public void dump( final Writer writer ) throws IOException
  {
    final SortedSet<IDMap> sort = new TreeSet<IDMap>( IDMap.COMPARATOR );
    
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
}

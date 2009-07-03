/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.convert.namodel.manager;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author doemming
 */
public abstract class AbstractManager
{
  private final static HashMap m_map = new HashMap(); // intID,StringID

  private static final HashMap m_allFeatures = new HashMap(); // (stringID,feature)

  private String[] m_asciiFormat;

  public String[] getAsciiFormats()
  {
    return m_asciiFormat;
  }

  public AbstractManager( URL parseDefinition ) throws IOException
  {
    if( parseDefinition != null )
      readParseDefinition( parseDefinition );

  }

  public Feature getFeature( String asciiStringId, FeatureType ft )
  {
    String fId = mapID( asciiStringId, ft );
    if( !m_allFeatures.containsKey( fId ) )
    {
      Feature feature = FeatureFactory.createFeature( fId, ft, false );
      m_allFeatures.put( fId, feature );
    }
    return (Feature)m_allFeatures.get( fId );
  }

  /**
   * maps the asciiStringId to the FeatureId
   */
  private String mapID( String asciiStringId, FeatureType ft )
  {
    return ft.getName() + "_" + asciiStringId;
  }

  public Feature getFeature( int asciiID, FeatureType ft )
  {
    IntID intID = new IntID( asciiID, ft );
    if( !m_map.containsKey( intID ) )
      createFeature( intID );
    String stringID = (String)m_map.get( intID );
    return (Feature)m_allFeatures.get( stringID );
  }

  public Feature getExistingFeature( int id, FeatureType[] ft )
  {
    for( int i = 0; i < ft.length; i++ )
    {
      Feature fe = getExistingFeature( id, ft[i] );
      if( fe != null )
        return fe;
    }
    return null;
  }

  public Feature getExistingFeature( int id, FeatureType ft )
  {
    IntID intID = new IntID( id, ft );
    String stringID = (String)m_map.get( intID );
    return (Feature)m_allFeatures.get( stringID );

  }

  private static int count = 0;

  public Feature createFeature( FeatureType ft )
  {
    String stringID = mapID( count++, ft );
    return FeatureFactory.createFeature( stringID, ft, false );

  }

  private void createFeature( IntID intID )
  {
    createMapping( intID );
    String stringID = (String)m_map.get( intID );
    Feature feature = null;
    try
    {
      feature = FeatureFactory.createFeature( stringID, intID.getFeatureType(), false );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    if( m_allFeatures.containsKey( stringID ) )
      throw new UnsupportedOperationException( "IDs are not unique:" + stringID );
    m_allFeatures.put( stringID, feature );
  }

  public abstract String mapID( int id, FeatureType ft );

  private void createMapping( IntID intID )
  {
    String stringID = mapID( intID.getID(), intID.getFeatureType() );
    m_map.put( stringID, intID );
    m_map.put( intID, stringID );
  }

  private void readParseDefinition( URL formatURL ) throws IOException
  {
    List result = new ArrayList();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( formatURL.openStream() ) );

    String line;
    while( ( line = reader.readLine() ) != null )
      result.add( line );
    m_asciiFormat = (String[])result.toArray( new String[result.size()] );
  }

  public abstract Feature[] parseFile( URL url ) throws Exception;

  public void createProperties( HashMap propCollector, String line, int formatLine ) throws Exception
  {
    createProperties( propCollector, line, m_asciiFormat[formatLine] );
  }

  protected void createProperties( HashMap propCollector, String line, String formatLine ) throws Exception
  {
    final HashMap propertyMap = FortranFormatHelper.scanf( formatLine, line );
    final Iterator it = propertyMap.keySet().iterator();
    while( it.hasNext() )
    {
      final String key = (String)it.next();
      propCollector.put( key, FeatureFactory.createFeatureProperty( key, propertyMap.get( key ) ) );
    }
  }

  public String toAscci( Feature feature, int formatLineIndex )
  {
    return ASCIIHelper.toAsciiLine( feature, m_asciiFormat[formatLineIndex] );
  }

  public void setParsedProperties( Feature feature, Collection collection )
  {
    FeatureType ft = feature.getFeatureType();

    Iterator it = collection.iterator();
    while( it.hasNext() )
    {
      FeatureProperty feProp = (FeatureProperty)it.next();
      if( ft.getProperty( feProp.getName() ) != null )
        feature.setProperty( feProp );
      else
        System.out.println( "property does not exist: >" + feProp.getName() + "=" + feProp.getValue() + "<" );
    }
  }

  private class IntID
  {
    private final int m_intID;

    private final FeatureType m_ft;

    public IntID( int intID, FeatureType ft )
    {
      m_intID = intID;
      m_ft = ft;
    }

    public int getID()
    {
      return m_intID;
    }

    public FeatureType getFeatureType()
    {
      return m_ft;
    }

    public boolean equals( Object object )
    {
      if( !( object instanceof IntID ) )
        return false;
      IntID other = (IntID)object;
      if( !( other.getID() == getID() ) )
        return false;
      if( !other.getFeatureType().getNamespace().equals( getFeatureType().getNamespace() ) )
        return false;
      if( !other.getFeatureType().getName().equals( getFeatureType().getName() ) )
        return false;
      return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    public int hashCode()
    {
      return ( Integer.toString( m_intID ) + m_ft.getName() + m_ft.getNamespace() ).hashCode();
    }

  }
}
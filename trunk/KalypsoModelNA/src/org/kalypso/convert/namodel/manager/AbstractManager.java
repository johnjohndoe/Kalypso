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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author doemming
 */
public abstract class AbstractManager
{
  private final static HashMap<IntID, String> m_map = new HashMap<IntID, String>();

  private static final HashMap<String, Feature> m_allFeatures = new HashMap<String, Feature>();

  private String[] m_asciiFormat;

  public String[] getAsciiFormats( )
  {
    return m_asciiFormat;
  }

  public AbstractManager( URL parseDefinition ) throws IOException
  {
    if( parseDefinition != null )
      readParseDefinition( parseDefinition );

  }

  public Feature getFeature( String asciiStringId, IFeatureType ft )
  {
    String fId = mapID( asciiStringId, ft );
    if( !m_allFeatures.containsKey( fId ) )
    {
      Feature feature = FeatureFactory.createFeature( null, null, fId, ft, false );
      m_allFeatures.put( fId, feature );
    }
    return m_allFeatures.get( fId );
  }

  /**
   * maps the asciiStringId to the FeatureId
   */
  private String mapID( String asciiStringId, IFeatureType ft )
  {
    return ft.getQName().getLocalPart() + "_" + asciiStringId; //$NON-NLS-1$
  }

  public Feature getFeature( int asciiID, IFeatureType ft )
  {
    IntID intID = new IntID( asciiID, ft );
    if( !m_map.containsKey( intID ) )
      createFeature( intID );
    String stringID = m_map.get( intID );
    return m_allFeatures.get( stringID );
  }

  public Feature getExistingFeature( int id, IFeatureType[] ft )
  {
    for( int i = 0; i < ft.length; i++ )
    {
      Feature fe = getExistingFeature( id, ft[i] );
      if( fe != null )
        return fe;
    }
    return null;
  }

  public Feature getExistingFeature( int id, IFeatureType ft )
  {
    IntID intID = new IntID( id, ft );
    String stringID = m_map.get( intID );
    return m_allFeatures.get( stringID );

  }

  private static int count = 0;

  public Feature createFeature( IFeatureType ft )
  {
    String stringID = mapID( count++, ft );
    return FeatureFactory.createFeature( null, null, stringID, ft, false );

  }

  private void createFeature( IntID intID )
  {
    createMapping( intID );
    String stringID = m_map.get( intID );
    Feature feature = null;
    try
    {
      feature = FeatureFactory.createFeature( null, null, stringID, intID.getFeatureType(), false );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    if( m_allFeatures.containsKey( stringID ) )
      throw new UnsupportedOperationException( "IDs are not unique:" + stringID );
    m_allFeatures.put( stringID, feature );
  }

  public abstract String mapID( int id, IFeatureType ft );

  private void createMapping( IntID intID )
  {
    String stringID = mapID( intID.getID(), intID.getFeatureType() );
    // m_map.put( stringID, intID );
    m_map.put( intID, stringID );
  }

  private void readParseDefinition( URL formatURL ) throws IOException
  {
    final List<String> result = new ArrayList<String>();
    final LineNumberReader reader = new LineNumberReader( new InputStreamReader( formatURL.openStream() ) );

    String line;
    while( (line = reader.readLine()) != null )
      result.add( line );
    m_asciiFormat = result.toArray( new String[result.size()] );
  }

  public abstract Feature[] parseFile( URL url ) throws Exception;

  public void createProperties( HashMap<String, String> propCollector, String line, int formatLine ) throws Exception
  {
    createProperties( propCollector, line, m_asciiFormat[formatLine] );
  }

  protected void createProperties( HashMap<String, String> propCollector, String line, String formatLine ) throws Exception
  {
    final HashMap<String, String> propertyMap = FortranFormatHelper.scanf( formatLine, line );
    final Iterator it = propertyMap.keySet().iterator();
    while( it.hasNext() )
    {
      final String key = (String) it.next();
      propCollector.put( key, propertyMap.get( key ) );
    }
  }

  public String toAscci( Feature feature, int formatLineIndex )
  {
    return ASCIIHelper.toAsciiLine( feature, m_asciiFormat[formatLineIndex] );
  }

  /**
   * this function sets alls properties in the feature, that can transformed to the propper typeclass, (e.g. String to
   * Integer), methodes with not simple types must call
   * <code>setParsedProperties( Feature feature, Collection<FeatureProperty> collection )</code>
   */
  public void setParsedProperties( final Feature feature, final HashMap<String, String> col )
  {
    final IFeatureType ft = feature.getFeatureType();
    final IPropertyType[] props = ft.getProperties();
    for( int i = 0; i < props.length; i++ )
    {
      if( props[i] instanceof IValuePropertyType )
      {
        final IValuePropertyType vpt = (IValuePropertyType) props[i];
        final Class clazz = vpt.getValueClass();
        final String value = col.get( vpt.getQName().getLocalPart() );
        if( clazz == String.class )
          feature.setProperty( vpt, value );
        else if( clazz == Integer.class )
          feature.setProperty( vpt, new Integer( value ) );
        else if( clazz == Float.class )
          feature.setProperty( vpt, new Float( value ) );
        else if( clazz == Long.class )
          feature.setProperty( vpt, new Long( value ) );
        else if( clazz == Double.class )
          feature.setProperty( vpt, new Double( value ) );
        else if( clazz == Boolean.class )
          feature.setProperty( vpt, new Boolean( value ) );
      }
    }
  }

  public void setParsedProperties( final Feature feature, final HashMap<String, String> mapCol, final Map<IPropertyType, Object> propertyValues )
  {
    if( mapCol != null )
      setParsedProperties( feature, mapCol );
    if( propertyValues != null )
      setParsedProperties( feature, propertyValues );
  }

  public void setParsedProperties( final Feature feature, final Map<IPropertyType, Object> values )
  {
    final IFeatureType ft = feature.getFeatureType();
    for( final Map.Entry<IPropertyType, Object> entry : values.entrySet() )
    {
      final IPropertyType propertyType = entry.getKey();
      final QName qname = propertyType.getQName();
      final Object value = entry.getValue();
      if( ft.getProperty( qname ) != null )
        feature.setProperty( propertyType, value );
      else
        System.out.println( "property does not exist: >" + qname.getLocalPart() + "=" + value + "<" ); //$NON-NLS-2$ //$NON-NLS-3$
    }
  }

  private class IntID
  {
    private final int m_intID;

    private final IFeatureType m_ft;

    public IntID( int intID, IFeatureType ft )
    {
      m_intID = intID;
      m_ft = ft;
    }

    public int getID( )
    {
      return m_intID;
    }

    public IFeatureType getFeatureType( )
    {
      return m_ft;
    }

    @Override
    public boolean equals( Object object )
    {
      if( !(object instanceof IntID) )
        return false;
      IntID other = (IntID) object;
      if( !(other.getID() == getID()) )
        return false;
      if( !other.getFeatureType().getQName().equals( getFeatureType().getQName() ) )
        return false;
      if( !other.getFeatureType().getQName().getLocalPart().equals( getFeatureType().getQName().getLocalPart() ) )
        return false;
      return true;
    }

    /**
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode( )
    {
      return (Integer.toString( m_intID ) + m_ft.getQName().getLocalPart() + m_ft.getQName()).hashCode();
    }

  }
}
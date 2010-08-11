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

  public AbstractManager( final URL parseDefinition ) throws IOException
  {
    if( parseDefinition != null )
      readParseDefinition( parseDefinition );

  }

  protected Feature getFeature( final String asciiStringId, final IFeatureType ft )
  {
    final String fId = mapID( asciiStringId, ft );
    if( !m_allFeatures.containsKey( fId ) )
    {
      final Feature feature = FeatureFactory.createFeature( null, null, fId, ft, false );
      m_allFeatures.put( fId, feature );
    }
    return m_allFeatures.get( fId );
  }

  /**
   * maps the asciiStringId to the FeatureId
   */
  private String mapID( final String asciiStringId, final IFeatureType ft )
  {
    return ft.getQName().getLocalPart() + "_" + asciiStringId; //$NON-NLS-1$
  }

  public Feature getFeature( final int asciiID, final IFeatureType ft )
  {
    final IntID intID = new IntID( asciiID, ft );
    if( !m_map.containsKey( intID ) )
      createFeature( intID );
    final String stringID = m_map.get( intID );
    return m_allFeatures.get( stringID );
  }

  public Feature getExistingFeature( final int id, final IFeatureType[] ft )
  {
    for( final IFeatureType element : ft )
    {
      final Feature fe = getExistingFeature( id, element );
      if( fe != null )
        return fe;
    }
    return null;
  }

  public Feature getExistingFeature( final int id, final IFeatureType ft )
  {
    final IntID intID = new IntID( id, ft );
    final String stringID = m_map.get( intID );
    return m_allFeatures.get( stringID );
  }

  private static int count = 0;

  public Feature createFeature( final IFeatureType ft )
  {
    final String stringID = mapID( count++, ft );
    return FeatureFactory.createFeature( null, null, stringID, ft, false );
  }

  private void createFeature( final IntID intID )
  {
    createMapping( intID );
    final String stringID = m_map.get( intID );
    Feature feature = null;
    try
    {
      feature = FeatureFactory.createFeature( null, null, stringID, intID.getFeatureType(), false );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    if( m_allFeatures.containsKey( stringID ) )
      throw new UnsupportedOperationException( "IDs are not unique:" + stringID ); //$NON-NLS-1$
    m_allFeatures.put( stringID, feature );
  }

  protected abstract String mapID( int id, IFeatureType ft );

  private void createMapping( final IntID intID )
  {
    final String stringID = mapID( intID.getID(), intID.getFeatureType() );
    // m_map.put( stringID, intID );
    m_map.put( intID, stringID );
  }

  private void readParseDefinition( final URL formatURL ) throws IOException
  {
    final List<String> result = new ArrayList<String>();
    final LineNumberReader reader = new LineNumberReader( new InputStreamReader( formatURL.openStream() ) );

    String line;
    while( (line = reader.readLine()) != null )
      result.add( line );
    m_asciiFormat = result.toArray( new String[result.size()] );
  }

  public abstract Feature[] parseFile( URL url ) throws Exception;

  protected void createProperties( final Map<String, String> propCollector, final String line, final int formatLine ) throws Exception
  {
    createProperties( propCollector, line, m_asciiFormat[formatLine] );
  }

  protected void createProperties( final Map<String, String> propCollector, final String line, final String formatLine ) throws Exception
  {
    final Map<String, String> propertyMap = FortranFormatHelper.scanf( formatLine, line );
    final Iterator<String> it = propertyMap.keySet().iterator();
    while( it.hasNext() )
    {
      final String key = it.next();
      propCollector.put( key, propertyMap.get( key ) );
    }
  }

  protected String toAscci( final Feature feature, final int formatLineIndex )
  {
    return ASCIIHelper.toAsciiLine( feature, m_asciiFormat[formatLineIndex] );
  }

  /**
   * this function sets alls properties in the feature, that can transformed to the propper typeclass, (e.g. String to
   * Integer), methodes with not simple types must call
   * <code>setParsedProperties( Feature feature, Collection<FeatureProperty> collection )</code>
   */
  private void setParsedProperties2( final Feature feature, final Map<String, String> col )
  {
    final IFeatureType ft = feature.getFeatureType();
    final IPropertyType[] props = ft.getProperties();
    for( final IPropertyType prop : props )
    {
      if( prop instanceof IValuePropertyType )
      {
        final IValuePropertyType vpt = (IValuePropertyType) prop;
        final Class< ? > clazz = vpt.getValueClass();
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

  protected void setParsedProperties( final Feature feature, final Map<String, String> mapCol, final Map<IPropertyType, Object> propertyValues )
  {
    if( mapCol != null )
      setParsedProperties2( feature, mapCol );
    if( propertyValues != null )
      setParsedProperties( feature, propertyValues );
  }

  private void setParsedProperties( final Feature feature, final Map<IPropertyType, Object> values )
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
        System.out.println( "property does not exist: >" + qname.getLocalPart() + "=" + value + "<" ); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
    }
  }

  private class IntID
  {
    private final int m_intID;

    private final IFeatureType m_ft;

    public IntID( final int intID, final IFeatureType ft )
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
    public boolean equals( final Object object )
    {
      if( !(object instanceof IntID) )
        return false;
      final IntID other = (IntID) object;
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
      // TODO: check was used like this in ModelWeißeElstter
// return new HashCodeBuilder().append( m_intID ).append( m_ft.getQName() ).toHashCode();
      return (Integer.toString( m_intID ) + m_ft.getQName().getLocalPart() + m_ft.getQName()).hashCode();
    }

  }
}
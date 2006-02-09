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
package org.kalypso.ogc.gml.convert.source.visitors;

import java.util.Properties;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Generiert TimeseriesLinks in einem Features anhand von String-Patterns.
 * 
 * <p>
 * Unterstützte Variablen für das Pattern:
 * </p>
 * <ul>
 * <li>${fid} Feature-ID</li>
 * <li>${fidOnlyDigits} Same as ${fid}, but all non-digits are pruned. </li>
 * <li>${property:<name>} Will be replaced with the String-Version of the property with name in the current feature. </li>
 * <li><${wiski_sim:link_property} Erstellt die Href anhand der Href der angegebenen Property. Diese wird nach dem Wiski-Simulations Pattern abgeändert. </li>
 * </ul>
 * 
 * @author belger
 */
public class TimeseriesLinkGenerateVisitor implements FeatureVisitor
{
  private static final String WISKI_SIM_PATTERN = "${wiski_sim:";
  private final ObjectFactory m_linkFactory = new ObjectFactory();

  private final String m_hrefPattern;

  private final String m_propertyName;

  /**
   * Dieser Konstruktor wird benötigt, um dat dingen auch aus den gmc Skripten heraus zu benutzen.
   */
  public TimeseriesLinkGenerateVisitor( final Properties properties )
  {
    this( properties.getProperty( "link", "<Link-Property nicht gesetzt>" ), properties.getProperty( "href", "" ) );
  }

  public TimeseriesLinkGenerateVisitor( final String propertyName, final String hrefPattern )
  {
    m_propertyName = propertyName;
    m_hrefPattern = hrefPattern;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final String href = applyPatterns( f );
    
    final TimeseriesLinkType link = m_linkFactory.createTimeseriesLinkType();
    link.setHref( href );
    final IPropertyType pt = FeatureHelper.getPT(f,m_propertyName);
    f.setProperty( FeatureFactory.createFeatureProperty( pt, link ) );

    return true;
  }

  private String applyPatterns( final Feature f )
  {
    // hrefPattern durch angaben ersetzen
    String href = m_hrefPattern;
    
    // Feature ID
    final String fid = f.getId();
    final String fidOnlyDigits = fid.replaceAll( "\\D", "" );
    
    href = href.replaceAll( "\\Q${fid}\\E", fid );
    href = href.replaceAll( "\\Q${fidOnlyDigits}\\E", fidOnlyDigits );
    href = replaceProperties( f, href );
    
    // wiski pattern
    href = applyWiskiSim( href, f );

    return href;
  }

  private String replaceProperties( final Feature f, final String href )
  {
    final String PROPERTY = "${property";

    String newHref = href;
    while( true )
    {
      final int start = newHref.indexOf( PROPERTY );
      if( start == -1 )
        break;
      
      final int stop = newHref.indexOf( '}', start + PROPERTY.length() + 1 );
      if( stop == -1 )
        break;
      
      final String name = newHref.substring( start + PROPERTY.length() + 1, stop );
      final Object value = f.getProperty( name );
      final String replacement = value == null ? "<null>" : value.toString();
      newHref = newHref.substring( 0, start ) + replacement + newHref.substring( stop + 1 );
    }
    
    return newHref;
  }

  private String applyWiskiSim( final String href, final Feature f )
  {
    String result = href;
    
    int index;
    while( ( index = result.indexOf( WISKI_SIM_PATTERN ) ) != -1 )
    {
      final int index2 = result.indexOf( "}", index + 1 );
      if( index2 == -1 )
        throw new IllegalArgumentException( "Pattern " + WISKI_SIM_PATTERN + "property} endet nicht mit }" );

      final String propertyName = result.substring( index + WISKI_SIM_PATTERN.length(), index2 );
      final Object property = f.getProperty( propertyName );
      
      final String simId;
      
      if( property == null )
        simId = "";
      else if( !( property instanceof TimeseriesLinkType ) )
        throw new IllegalArgumentException( "Property ist kein TimeseriesLinkType: " + propertyName );
      else
      {
        final TimeseriesLinkType link = (TimeseriesLinkType)property;
        final String oldHref = link.getHref();
        simId = makeWiskiSimHref( oldHref );
      }
      
      result = result.substring( 0, index ) + simId + result.substring( index2 + 1 );
    }
    
    return result;
  }

  /** Aus 'blubb.X.nn' wird 'blubb.Sim.X.nn' */
  private String makeWiskiSimHref( final String href )
  {
    final int lastPoint = href.lastIndexOf( '.' );
    if( lastPoint == -1 )
      return "";
    
    final String name = href.substring( 0, lastPoint );
    final String tail = href.substring( lastPoint );
    
    return name + ".Sim" + tail;
  }
}

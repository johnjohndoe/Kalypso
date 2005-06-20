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
package org.kalypso.ogc.gml.convert.source;

import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.kalypso.gml.util.ChangeSourceType;
import org.kalypso.gml.util.ChangeSourceType.VisitorType;
import org.kalypso.gml.util.ChangeSourceType.VisitorType.ArgumentType;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.java.lang.reflect.ClassUtilityException;
import org.kalypso.java.net.IUrlResolver;
import org.kalypso.ogc.gml.convert.GmlConvertException;
import org.kalypso.ogc.gml.convert.GmlConvertFactory;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author belger
 */
public class ChangeSourceTypeHandler implements ISourceHandler
{
  private final IUrlResolver m_resolver;

  private final ChangeSourceType m_type;

  private final URL m_context;

  private final Map m_externData;

  public ChangeSourceTypeHandler( final IUrlResolver resolver, final URL context, final ChangeSourceType type,
      final Map externData )
  {
    m_resolver = resolver;
    m_context = context;
    m_type = type;
    m_externData = externData;
  }

  /**
   * @see org.kalypso.ogc.gml.convert.source.ISourceHandler#getWorkspace()
   */
  public GMLWorkspace getWorkspace() throws GmlConvertException
  {
    try
    {
      final GMLWorkspace inputGML = GmlConvertFactory.loadSource( m_resolver, m_context, m_type.getSource(),
          m_externData );
      applyVisitors( inputGML, m_type.getVisitor() );
      return inputGML;
    }
    catch( final ClassUtilityException e )
    {
      throw new GmlConvertException( "Features konnten nicht ge�ndert werden.", e );
    }
  }

  private void applyVisitors( final GMLWorkspace inputGML, final List propertyList ) throws ClassUtilityException
  {
    final ClassLoader classLoader = getClass().getClassLoader();
    for( Iterator iter = propertyList.iterator(); iter.hasNext(); )
    {
      final ChangeSourceType.VisitorType visitorType = (VisitorType)iter.next();
      final String featurePath = visitorType.getFeaturePath();
      final String visitorClass = visitorType.getVisitorclass();

      final Properties arguments = createArguments( visitorType.getArgument() );

      final FeatureVisitor visitor = (FeatureVisitor)ClassUtilities.newInstance( visitorClass, FeatureVisitor.class,
          classLoader, new Object[]
          { arguments } );

      inputGML.accept( visitor, featurePath, FeatureVisitor.DEPTH_INFINITE );
    }
  }

  private Properties createArguments( final List argumentList )
  {
    final Properties map = new Properties();

    for( Iterator iter = argumentList.iterator(); iter.hasNext(); )
    {
      final ChangeSourceType.VisitorType.ArgumentType aType = (ArgumentType)iter.next();
      map.setProperty( aType.getName(), aType.getValue() );
    }

    return map;
  }
}

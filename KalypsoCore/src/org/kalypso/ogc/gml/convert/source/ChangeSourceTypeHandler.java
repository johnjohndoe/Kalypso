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

import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.contribs.java.lang.reflect.ClassUtilityException;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.core.KalypsoCoreExtensions;
import org.kalypso.gml.util.ChangeSourceType;
import org.kalypso.gml.util.ChangeSourceType.VisitorType;
import org.kalypso.gml.util.ChangeSourceType.VisitorType.ArgumentType;
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
      throw new GmlConvertException( "Features konnten nicht geändert werden.", e );
    }
  }

  private void applyVisitors( final GMLWorkspace inputGML, final List propertyList ) throws ClassUtilityException,
      GmlConvertException
  {
    for( final Iterator iter = propertyList.iterator(); iter.hasNext(); )
    {
      final ChangeSourceType.VisitorType visitorType = (VisitorType)iter.next();
      final String featurePath = visitorType.getFeaturePath();
      final Properties arguments = createArguments( visitorType.getArgument() );

      final String visitorClass = visitorType.getVisitorclass();
      final String visitorID = visitorType.getVisitorid();
      final FeatureVisitor visitor = createVisitor( visitorClass, visitorID, arguments );
      inputGML.accept( visitor, featurePath, FeatureVisitor.DEPTH_INFINITE );
    }
  }

  private FeatureVisitor createVisitor( final String visitorClass, final String visitorID, final Properties arguments ) throws ClassUtilityException, GmlConvertException
  {
    if( visitorClass != null )
    {
      final ClassLoader classLoader = getClass().getClassLoader();

      return (FeatureVisitor)ClassUtilities.newInstance( visitorClass, FeatureVisitor.class, classLoader, new Object[]
      { arguments } );
    }
    else if( visitorID != null )
    {
      try
      {
        final Properties properties = new Properties();
        properties.put( "context", m_context.toExternalForm() );
        properties.putAll( arguments );

        return KalypsoCoreExtensions.createFeatureVisitor( visitorID, properties );
      }
      catch( final CoreException e )
      {
        throw new GmlConvertException( e );
      }
    }

    throw new GmlConvertException( "Either visitorClass or visitorID must be set" );
  }

  private Properties createArguments( final List argumentList )
  {
    final Properties map = new Properties();

    for( final Iterator iter = argumentList.iterator(); iter.hasNext(); )
    {
      final ChangeSourceType.VisitorType.ArgumentType aType = (ArgumentType)iter.next();
      map.setProperty( aType.getName(), aType.getValue() );
    }

    return map;
  }
}

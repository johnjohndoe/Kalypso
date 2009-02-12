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
package org.kalypso.services.calculation.local;

import java.net.URL;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.kalypso.contribs.java.net.IUrlCatalog;
import org.kalypso.contribs.java.net.MultiUrlCatalog;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypso.services.calculation.service.impl.ICalcJobFactory;

/**
 * Lädt alle als EclipsePlugins geladenen Calculation-Plugins anhand des Extension-Points.
 * 
 * @author belger
 */
public class CalcPluginExtensionPointReader implements IUrlCatalog, ICalcJobFactory
{
  /** typeID -> configurationElement */
  private final Map m_elementHash = new HashMap();

  private MultiUrlCatalog m_catalog;

  public CalcPluginExtensionPointReader()
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IExtensionPoint point = registry.getExtensionPoint( KalypsoLocalCalculationPlugin.getDefault().getId(),
        IKalypsoLocalCalculationConstants.EXT_ELEMENT_CALCJOB );
//    System.out.println( getClass().getName() + " ist extensionpoint da ?" );
    if( point == null )
    {
//      System.out.println( getClass().getName() + " ... NEE" );
      return;
    }
//    System.out.println( getClass().getName() + " ... OK" );

    final List catalogs = new LinkedList();

    final IExtension[] extensions = point.getExtensions();
//    System.out.println( getClass().getName() + "size extensions:"+extensions.length );
    for( int i = 0; i < extensions.length; i++ )
    {
      final IExtension extension = extensions[i];
      final IConfigurationElement[] configurationElements = extension.getConfigurationElements();
//      System.out.println( getClass().getName() + "size confElements:"+configurationElements.length );
      for( int j = 0; j < configurationElements.length; j++ )
      {
        final IConfigurationElement element = configurationElements[j];
//        System.out.println( getClass().getName() + "element :"+element.getName());
        final String typeID = element.getAttribute( IKalypsoLocalCalculationConstants.EXT_ATTRIB_ID );
        
//        System.out.println( getClass().getName() + "typeID :"+typeID);
        m_elementHash.put( typeID, element );

        try
        {
          final IUrlCatalog catalog = (IUrlCatalog)element
              .createExecutableExtension( IKalypsoLocalCalculationConstants.EXT_ATTRIB_CATALOGCLASS );
          catalogs.add( catalog );
        }
        catch( final CoreException e )
        {
          e.printStackTrace();
        }
      }
    }

    m_catalog = new MultiUrlCatalog( (IUrlCatalog[])catalogs.toArray( new IUrlCatalog[catalogs.size()] ) );
  }

  /**
   * @see org.kalypso.contribs.java.net.IUrlCatalog#getURL(java.lang.String)
   */
  public URL getURL( final String namespace )
  {
    return m_catalog.getURL( namespace );
  }

  /**
   * @see org.kalypso.contribs.java.net.IUrlCatalog#getCatalog()
   */
  public Map getCatalog()
  {
    return m_catalog.getCatalog();
  }

  /**
   * @see org.kalypso.services.calculation.service.impl.ICalcJobFactory#getSupportedTypes()
   */
  public String[] getSupportedTypes()
  {
    return (String[])m_elementHash.keySet().toArray( new String[m_elementHash.size()] );
  }

  /**
   * @see org.kalypso.services.calculation.service.impl.ICalcJobFactory#createJob(java.lang.String)
   */
  public ICalcJob createJob( final String typeID ) throws CalcJobServiceException
  {
    final IConfigurationElement element = (IConfigurationElement)m_elementHash.get( typeID );
    if( element == null )
      return null;

    try
    {
      return (ICalcJob)element.createExecutableExtension( IKalypsoLocalCalculationConstants.EXT_ATTRIB_JOBCLASS );
    }
    catch( CoreException e )
    {
      e.printStackTrace();

      throw new CalcJobServiceException( "Job-Klasse konnte nicht instantiiet werden für typ: " + typeID, e );
    }
  }
}

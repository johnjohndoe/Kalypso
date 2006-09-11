/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypsodeegree;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * Helper class to read extension-points of this plugin.
 * 
 * @author belger
 */
public class KalypsoDeegreeExtensions
{
  private final static String FUNCTION_EXTENSION_POINT = "org.kalypso.deegree.functionProperty";

  private static Map<String, IConfigurationElement> FUNCTION_MAP = null;

  public static synchronized FeaturePropertyFunction createPropertyFunction( final String id, final Map<String, String> properties ) throws CoreException
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    final IExtensionPoint extensionPoint = registry.getExtensionPoint( FUNCTION_EXTENSION_POINT );
    if( FUNCTION_MAP == null )
    {
      final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
      FUNCTION_MAP = new HashMap<String, IConfigurationElement>( configurationElements.length );
      for( int i = 0; i < configurationElements.length; i++ )
      {
        final IConfigurationElement element = configurationElements[i];
        final String configid = element.getAttribute( "id" );
        FUNCTION_MAP.put( configid, element );
      }
    }

    if( !FUNCTION_MAP.containsKey( id ) )
    {
      final IStatus status = StatusUtilities.createErrorStatus( "No function property with id: " + id );
      throw new CoreException( status );
    }

    final IConfigurationElement element = FUNCTION_MAP.get( id );
    final FeaturePropertyFunction function = (FeaturePropertyFunction) element.createExecutableExtension( "class" );
    function.init( properties );

    return function;
  }
}

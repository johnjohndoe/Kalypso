/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.util.pool;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Helper class to read and cache
 * 
 * @author Stefan Kurzbach
 */
public class ModelAdapterExtension
{
  private final static String MODEL_ADAPTER_EXTENSION_POINT = "org.kalypso.ui.modelAdapter"; //$NON-NLS-1$

  private static final String ATTRIBUTE_ADAPTER_CLASS = "adapterClass"; //$NON-NLS-1$

  private static final String ATTRIBUTE_FEATURE_TYPE = "featureType"; //$NON-NLS-1$

  public static IModelAdaptor[] getModelAdaptor( final String featureType )
  {
    final IModelAdaptor[] map = getModelAdaptorList( featureType );
    return map;
  }

  private static IModelAdaptor[] getModelAdaptorList( final String featureType )
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IExtensionPoint extensionPoint = registry.getExtensionPoint( MODEL_ADAPTER_EXTENSION_POINT );
    final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();

    final List<IModelAdaptor> result = new ArrayList<IModelAdaptor>();
    for( final IConfigurationElement element : configurationElements )
    {
      final String elFeatureType = element.getAttribute( ATTRIBUTE_FEATURE_TYPE );
      if( featureType.equals( elFeatureType ) )
      {
        try
        {
          final IModelAdaptor adaptor = (IModelAdaptor) element.createExecutableExtension( ATTRIBUTE_ADAPTER_CLASS );
          result.add( adaptor );
        }
        catch( final CoreException e )
        {
          KalypsoGisPlugin.getDefault().getLog().log( e.getStatus() );
          e.printStackTrace();
        }
      }
    }

    return result.toArray( new IModelAdaptor[result.size()] );
  }

}

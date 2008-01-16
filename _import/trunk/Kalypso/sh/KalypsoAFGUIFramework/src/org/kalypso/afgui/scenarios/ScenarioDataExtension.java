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
package org.kalypso.afgui.scenarios;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;

/**
 * Helper class to read and cache
 * 
 * @author Stefan Kurzbach
 */
public class ScenarioDataExtension
{
  private final static String SCENARIO_DATA_EXTENSION_POINT = "org.kalypso.afgui.scenarioData"; //$NON-NLS-1$

  private static final String ATTRIBUTE_MODEL_PATH = "modelPath"; //$NON-NLS-1$

  private static final String ATTRIBUTE_CLASS_KEY = "classKey"; //$NON-NLS-1$

  private static final String ATTRIBUTE_ID = "id"; //$NON-NLS-1$

  private static Map<String, String> CLASS_LOADER_MAP;

  public static String getScenarioDataPath( final String id, final Class< ? extends IModel> classKey )
  {
    final Map<Class< ? extends IModel>, String> map = getLocationMap( id );

    return map.get( classKey );
  }

  @SuppressWarnings("unchecked")
  public static Map<Class< ? extends IModel>, String> getLocationMap( final String id )
  {
    if( CLASS_LOADER_MAP == null )
    {
      CLASS_LOADER_MAP = new HashMap<String, String>();
    }
    CLASS_LOADER_MAP.clear();

    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IExtensionPoint extensionPoint = registry.getExtensionPoint( SCENARIO_DATA_EXTENSION_POINT );
    final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();

    final Map<Class< ? extends IModel>, String> dataSetMap = new HashMap<Class< ? extends IModel>, String>();
    for( final IConfigurationElement dataSet : configurationElements )
    {
      final String dataSetId = dataSet.getAttribute( ATTRIBUTE_ID );
      if( dataSetId.equals( id ) )
      {
        final IConfigurationElement[] elements = dataSet.getChildren();
        for( final IConfigurationElement element : elements )
        {
          final String classKeyName = element.getAttribute( ATTRIBUTE_CLASS_KEY );
          final String bundleId = element.getContributor().getName();
          CLASS_LOADER_MAP.put( classKeyName, bundleId );

          final String modelPath = element.getAttribute( ATTRIBUTE_MODEL_PATH );
          final Class< ? extends IModel> classKey = loadClass( classKeyName );
          dataSetMap.put( classKey, modelPath );
        }
      }
    }

    return dataSetMap;
  }

  public static Class<IModel> loadClass( final String classKey )
  {
    try
    {
      // TODO: don't! This violates the eclipse's lazy plug-in loading contract, as this
      // causes plug-ins to load that are not needed at the moment (only 'id' is called for)
      return Platform.getBundle( CLASS_LOADER_MAP.get( classKey ) ).loadClass( classKey );
    }
    catch( final ClassNotFoundException e )
    {
      e.printStackTrace();
    }
    return null;
  }

}

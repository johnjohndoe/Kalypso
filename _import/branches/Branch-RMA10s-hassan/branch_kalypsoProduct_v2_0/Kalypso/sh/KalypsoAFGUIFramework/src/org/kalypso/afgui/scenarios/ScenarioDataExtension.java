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
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.dynamichelpers.ExtensionTracker;
import org.eclipse.core.runtime.dynamichelpers.IExtensionChangeHandler;
import org.eclipse.core.runtime.dynamichelpers.IExtensionTracker;
import org.eclipse.core.runtime.dynamichelpers.IFilter;

/**
 * Helper class to read and cache
 * 
 * @author Stefan Kurzbach
 */
public class ScenarioDataExtension
{
  private final static String SCENARIO_DATA_EXTENSION_POINT = "org.kalypso.afgui.scenarioData"; //$NON-NLS-1$

  private static final String ATTRIBUTE_ID = "id"; //$NON-NLS-1$

  private static final Map<String, Map<String, IScenarioDatum>> m_dataSetCache = new HashMap<String, Map<String, IScenarioDatum>>();

  private static ExtensionTracker extensionTracker;

  public static String getScenarioDataPath( final String id, final String classKey )
  {
    final Map<String, IScenarioDatum> map = getScenarioDataMap( id );
    return map.get( classKey ).getModelPath();
  }

  public static Map<String, IScenarioDatum> getScenarioDataMap( final String id )
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IExtensionPoint extensionPoint = registry.getExtensionPoint( SCENARIO_DATA_EXTENSION_POINT );

    registerExtensionTracker( extensionPoint );

    Map<String, IScenarioDatum> scenarioDataMap = null;
    synchronized( m_dataSetCache )
    {
      scenarioDataMap = m_dataSetCache.get( id );
    }

    if( scenarioDataMap == null )
    {
      // create new map
      scenarioDataMap = new HashMap<String, IScenarioDatum>();

      // read from extension point registry
      final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();

      for( final IConfigurationElement dataSet : configurationElements )
      {
        final String dataSetId = dataSet.getAttribute( ATTRIBUTE_ID );
        if( dataSetId.equals( id ) )
        {
          final IConfigurationElement[] elements = dataSet.getChildren();
          for( final IConfigurationElement element : elements )
          {
            final String classKey = element.getAttribute( ScenarioDatumProxy.ATTRIBUTE_CLASS_KEY );
            final ScenarioDatumProxy scenarioDatum = new ScenarioDatumProxy( element );
            scenarioDataMap.put( classKey, scenarioDatum );
          }
        }
      }
      m_dataSetCache.put( id, scenarioDataMap );
    }

    return scenarioDataMap;
  }

  private static void registerExtensionTracker( final IExtensionPoint extensionPoint )
  {
    if( extensionTracker == null )
    {
      extensionTracker = new ExtensionTracker();
      extensionTracker.registerHandler( new ExtensionChangeHandler(), new IFilter()
      {
        public boolean matches( final IExtensionPoint target )
        {
          return extensionPoint.equals( target );
        }
      } );
    }
  }

  /* protected */static void extensionsInvalid( )
  {
    synchronized( m_dataSetCache )
    {
      m_dataSetCache.clear();
    }
  }

  /* protected */static class ExtensionChangeHandler implements IExtensionChangeHandler
  {

    /**
     * @see org.eclipse.core.runtime.dynamichelpers.IExtensionChangeHandler#addExtension(org.eclipse.core.runtime.dynamichelpers.IExtensionTracker,
     *      org.eclipse.core.runtime.IExtension)
     */
    public void addExtension( final IExtensionTracker tracker, final IExtension extension )
    {
      extensionsInvalid();
    }

    /**
     * @see org.eclipse.core.runtime.dynamichelpers.IExtensionChangeHandler#removeExtension(org.eclipse.core.runtime.IExtension,
     *      java.lang.Object[])
     */
    public void removeExtension( final IExtension extension, final Object[] objects )
    {
      extensionsInvalid();
    }
  }
}

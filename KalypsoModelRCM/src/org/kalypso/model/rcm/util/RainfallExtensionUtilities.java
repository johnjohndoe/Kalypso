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
package org.kalypso.model.rcm.util;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.rcm.internal.KalypsoModelRcmActivator;

/**
 * This class contains functions for dealing with code in plugins higher in the dependency hirachy.
 * 
 * @author Holger Albert
 */
public final class RainfallExtensionUtilities
{
  private static final String RAINFALL_CONFIGURATOR_EXTENSION_POINT = "org.kalypso.model.rcm.rainfallConfigurator";

  private static final String RAINFALL_CONFIGURATOR_CONFIGURATOR_ELEMENT = "configurator";

  private static final String OMBROMETER_GENERATOR_STRATEGY_CONFIGURATOR_ELEMENT = "ombrometerGeneratorStrategy"; //$NON-NLS-1$

  private static final String ATTRIBUTE_ID = "id"; //$NON-NLS-1$

  private static final String ATTRIBUTE_CLASS = "class"; //$NON-NLS-1$

  public static final String RAINFALL_CONFIGURATOR_ID = "org.kalypso.hwv.ui.utils.RdbRainfallConfigurator";

  private RainfallExtensionUtilities( )
  {
  }

  /**
   * This function creates and returns the rainfall configurator with the given id, if one is registered.
   * 
   * @param id
   *          The id of the rainfall configurator.
   * @return The rainfall configurator.
   */
  public static IRainfallConfigurator createRainfallConfigurator( final String id ) throws CoreException
  {
    /* Assert. */
    Assert.isNotNull( id );

    /* Get the extension registry. */
    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    /* Get the extension point. */
    final IExtensionPoint extensionPoint = registry.getExtensionPoint( RAINFALL_CONFIGURATOR_EXTENSION_POINT );

    /* Get all configuration elements. */
    final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
    for( final IConfigurationElement element : configurationElements )
    {
      /* If the configuration element is not the configurator element, continue. */
      if( !RAINFALL_CONFIGURATOR_CONFIGURATOR_ELEMENT.equals( element.getName() ) )
        continue;

      /* Get the attributes. */
      final String configuratorId = element.getAttribute( ATTRIBUTE_ID );
      if( id.equals( configuratorId ) )
        return (IRainfallConfigurator) element.createExecutableExtension( ATTRIBUTE_CLASS );
    }

    final String message = String.format( "Keinen Rainfall-Configurator mit ID '%s' gefunden.", id );
    throw new CoreException( new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, message ) );
  }

  /**
   * This function creates and returns the ombrometer generator strategy with the given id, if one is registered.
   * 
   * @param id
   *          The id of the ombrometer generator strategy.
   * @return The ombrometer generator strategy.
   */
  public static IOmbrometerGeneratorStrategy createOmbrometerGeneratorStrategy( final String id ) throws CoreException
  {
    /* Assert. */
    Assert.isNotNull( id );

    /* Get the extension registry. */
    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    /* Get the extension point. */
    final IExtensionPoint extensionPoint = registry.getExtensionPoint( RAINFALL_CONFIGURATOR_EXTENSION_POINT );

    /* Get all configuration elements. */
    final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
    for( final IConfigurationElement element : configurationElements )
    {
      /* If the configuration element is not the configurator element, continue. */
      if( OMBROMETER_GENERATOR_STRATEGY_CONFIGURATOR_ELEMENT.equals( element.getName() ) )
      {
        /* Get the attributes. */
        final String configuratorId = element.getAttribute( ATTRIBUTE_ID );
        if( id.equals( configuratorId ) )
          return (IOmbrometerGeneratorStrategy) element.createExecutableExtension( ATTRIBUTE_CLASS );
      }
    }

    final String message = String.format( "Keine Strategie mit ID '%s' gefunden.", id );
    throw new CoreException( new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, message ) );
  }
}
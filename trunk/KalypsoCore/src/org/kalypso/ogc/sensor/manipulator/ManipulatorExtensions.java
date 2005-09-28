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

package org.kalypso.ogc.sensor.manipulator;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;

/**
 * Handles the extensions for observationManipulator as instanceof IObservationManipulator
 * 
 * @author schlienger
 */
public class ManipulatorExtensions
{
  public final static String MANIPULATOR_EXTENSION_POINT = "org.kalypso.core.obsManipulator";

  private ManipulatorExtensions()
  {
  // not intended to be instanciated
  }

  /**
   * @return the first provider found or null if none
   */
  public static IObservationManipulator[] getManipulators()
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    if( registry == null ) // e.g. if we are in a (non-plugin-) junit-test
      return new IObservationManipulator[0];

    final IExtensionPoint extensionPoint = registry.getExtensionPoint( MANIPULATOR_EXTENSION_POINT );
    final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();

    if( configurationElements.length == 0 )
      return new IObservationManipulator[0];

    final List manips = new ArrayList( configurationElements.length );

    for( int i = 0; i < configurationElements.length; i++ )
    {
      final IConfigurationElement element = configurationElements[i];

      try
      {
        manips.add( element.createExecutableExtension( "class" ) );
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
      }
    }

    return (IObservationManipulator[])manips.toArray( new IObservationManipulator[manips.size()] );
  }

  /**
   * Uses the manipulators found as extensions and let them all manipulate the given observation
   */
  public static void manipulateObservation( final IObservation obs, final Object data ) throws SensorException
  {
    final IObservationManipulator[] manipulators = getManipulators();

    for( int i = 0; i < manipulators.length; i++ )
      manipulators[i].manipulate( obs, data );
  }
}

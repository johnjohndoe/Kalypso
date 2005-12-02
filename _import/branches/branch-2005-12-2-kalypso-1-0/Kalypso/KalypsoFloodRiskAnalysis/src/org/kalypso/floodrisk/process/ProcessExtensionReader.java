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
package org.kalypso.floodrisk.process;

import java.util.Vector;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.kalypso.services.calculation.job.ICalcJob;

/**
 * ProcessExtensionReader
 * <p>
 * 
 * created by
 * 
 * @author Nadja Peiler (13.05.2005)
 */
public class ProcessExtensionReader
{

  private static String ADDPROCESS_EXTENSION_POINT = "org.kalypso.floodrisk.AddProcess";

  private static String ATT_NAME = "name";

  private static String ATT_TYPE = "type";

  private static String ATT_CLASS = "class";

  private static String ATT_ID = "id";

  private static String AT_ICON = "icon";

  /**
   * Uses the platform extension registry to retrieve all extensions for the addProcess extension point.
   * <p>
   * For each extension, a ProcessExtension is created
   * 
   * @throws CoreException
   */
  public static ProcessExtension[] retrieveExtensions() throws CoreException
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    /*
     * IExtensionPoint[] extPoints = registry.getExtensionPoints();
     * 
     * for (int i = 0;i <extPoints.length;i++){ System.out.println(extPoints[i].getUniqueIdentifier()); }
     */
    final IExtensionPoint extensionPoint = registry.getExtensionPoint( ADDPROCESS_EXTENSION_POINT );

    if( extensionPoint == null )
      return new ProcessExtension[0];

    final IExtension[] extensions = extensionPoint.getExtensions();

    final Vector items = new Vector();

    for( int i = 0; i < extensions.length; i++ )
    {
      final IExtension extension = extensions[i];
      final IConfigurationElement[] elements = extension.getConfigurationElements();

      for( int j = 0; j < elements.length; j++ )
      {
        final IConfigurationElement element = elements[j];

        final String name = element.getAttribute( ATT_NAME );
        final String type = element.getAttribute( ATT_TYPE );
        final ICalcJob calcJob = (ICalcJob)element.createExecutableExtension( ATT_CLASS );
        final String id = element.getAttribute( ATT_ID );
        final String icon = element.getAttribute( AT_ICON );

        items.add( new ProcessExtension( name, type, calcJob, id, icon ) );
      }
    }

    return (ProcessExtension[])items.toArray( new ProcessExtension[items.size()] );
  }
}
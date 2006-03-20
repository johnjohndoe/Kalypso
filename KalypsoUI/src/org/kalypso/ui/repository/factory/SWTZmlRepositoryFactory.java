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
package org.kalypso.ui.repository.factory;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.ogc.sensor.zml.repository.HeadlessZmlRepositoryFactory;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * A GUI oriented File- <code>RepositoryFactory</code>. Please note that this factory currently creates a
 * <code>ZmlObservationRepository</code>. This could be changed to some other subclass of <code>FileRepository</code>
 * as long as the constructor sticks to the arguments used here. To achieve more flexibility, this class could be
 * improved so that the concrete <code>FileRepository</code> class to instantiate could be parametrised.
 * 
 * @author schlienger
 */
public class SWTZmlRepositoryFactory extends HeadlessZmlRepositoryFactory
{
  /**
   * @see org.kalypso.repository.factory.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository()
  {
    final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

    final FileRepositoryConfigDialog dlg = new FileRepositoryConfigDialog( shell, "", "", "", KalypsoGisPlugin
        .getDefault() );

    final int res = dlg.open();

    boolean b = false;

    if( res == Window.OK )
    {
      // update configuration to stay consistent
      setConfiguration( dlg.getLocation() + SEPARATOR + dlg.getFilters() );

      setRepositoryName( dlg.getIdentifier() );

      b = true;
    }

    dlg.dispose();

    return b;
  }
}
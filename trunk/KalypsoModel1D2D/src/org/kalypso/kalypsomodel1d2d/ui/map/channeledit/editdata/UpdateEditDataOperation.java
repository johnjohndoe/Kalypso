/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class UpdateEditDataOperation implements ICoreRunnableWithProgress
{
  private final ChannelEditProfileData m_oldData;

  private final ChannelEditProfileData m_newData;

  public UpdateEditDataOperation( final ChannelEditProfileData oldData, final ChannelEditProfileData newData )
  {
    m_oldData = oldData;
    m_newData = newData;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final String taskName = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.0" ); //$NON-NLS-1$

    monitor.beginTask( taskName, 0 );

    // FIXME compare with old data and determine data loss
    // FIXME: copy user-edited data from old data, if possible
    m_newData.initData();

    return Status.OK_STATUS;
  }

  public boolean hasDataLoss( )
  {
    // TODO Auto-generated method stub
    return false;
  }
}
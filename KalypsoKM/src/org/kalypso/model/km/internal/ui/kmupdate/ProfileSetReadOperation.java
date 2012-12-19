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
package org.kalypso.model.km.internal.ui.kmupdate;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.km.internal.KMPlugin;
import org.kalypso.model.km.internal.core.ProfileDataSet;
import org.kalypso.model.km.internal.core.ProfileObservationReader;
import org.kalypso.model.km.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class ProfileSetReadOperation implements ICoreRunnableWithProgress
{
  private final String m_path;

  private ProfileDataSet m_profileSet;

  public ProfileSetReadOperation( final String path )
  {
    m_path = path;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    if( StringUtils.isBlank( m_path ) )
      return Status.OK_STATUS;

    try
    {
      final ProfileObservationReader reader = new ProfileObservationReader( new Path( m_path ) );
      m_profileSet = reader.getDataSet();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, KMPlugin.getID(), Messages.getString("ProfileSetReadOperation_0"), e ); //$NON-NLS-1$
    }
    finally
    {
      monitor.done();
    }

    return Status.OK_STATUS;
  }

  public ProfileDataSet getProfileSet( )
  {
    return m_profileSet;
  }
}
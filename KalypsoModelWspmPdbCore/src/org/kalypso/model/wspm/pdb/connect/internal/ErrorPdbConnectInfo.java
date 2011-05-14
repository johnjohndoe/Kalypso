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
package org.kalypso.model.wspm.pdb.connect.internal;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.model.wspm.pdb.connect.IPdbConnectInfo;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;

/**
 * Implementation of {@link IPdbConnectInfo} that works as a placeholder, if a original info could not be read.
 * 
 * @author Gernot Belger
 */
public class ErrorPdbConnectInfo implements IPdbConnectInfo
{
  static final String TYPE = "error"; //$NON-NLS-1$

  private final IStatus m_status;

  public ErrorPdbConnectInfo( final IStatus status )
  {
    m_status = status;
  }

  @Override
  public String getType( )
  {
    return TYPE;
  }

  @Override
  public String getLabel( )
  {
    return StatusUtilities.getLocalizedSeverity( m_status );
  }

  @Override
  public ImageDescriptor getImage( )
  {
    return StatusComposite.getStatusImageDescriptor( m_status.getSeverity() );
  }

  @Override
  public IPdbConnection createConnection( )
  {
    // Connection not possible
    throw new NotImplementedException();
  }

  @Override
  public void saveState( final ISecurePreferences preferences )
  {
    // TODO: we should save the old state
  }


  @Override
  public void readState( final ISecurePreferences preferences )
  {
    // TODO: we should somehow copy the state
  }

  @Override
  public String toString( )
  {
    return m_status.getMessage();
  }

  @Override
  public IPdbConnectInfo copy( )
  {
    return new ErrorPdbConnectInfo( m_status );
  }

  @Override
  public Control createEditControl( final DataBindingContext binding, final Composite parent )
  {
    final StatusComposite statusControl = new StatusComposite( null, parent, StatusComposite.DETAILS );
    statusControl.setStatus( m_status );
    return statusControl;
  }
}
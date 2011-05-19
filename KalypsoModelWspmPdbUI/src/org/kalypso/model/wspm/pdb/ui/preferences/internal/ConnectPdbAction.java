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
package org.kalypso.model.wspm.pdb.ui.preferences.internal;

import org.apache.commons.lang.StringUtils;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.PdbSettings;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages;

/**
 * @author Gernot Belger
 */
public class ConnectPdbAction extends Action
{
  private final PdbView m_view;

  public ConnectPdbAction( final PdbView view )
  {
    super( "Open Connection..." );

    m_view = view;

    final ImageDescriptor image = WspmPdbUiImages.getImageDescriptor( WspmPdbUiImages.IMAGE.CONNECT_TO_PDB );
    setImageDescriptor( image );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final OpenConnectionData data = new OpenConnectionData();
    try
    {
      final String autoConnect = m_view.getAutoConnectName();
      final boolean isAutoConnect = !StringUtils.isBlank( autoConnect );
      data.setAutoConnect( isAutoConnect );
      final IPdbSettings autoSettings = PdbSettings.getSettings( autoConnect );
      data.setSettings( autoSettings );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
    }

    final Shell shell = m_view.getSite().getShell();
    final OpenConnectWizard wizard = new OpenConnectWizard( data );
    final WizardDialog dialog = new WizardDialog( shell, wizard );
    if( dialog.open() == Window.OK )
    {
      final String autoConnectName = wizard.getAutoConnectName();
      m_view.setAutoConnect( autoConnectName );

      final IPdbConnection connection = wizard.getConnection();
      m_view.setConnection( connection );
    }
  }
}
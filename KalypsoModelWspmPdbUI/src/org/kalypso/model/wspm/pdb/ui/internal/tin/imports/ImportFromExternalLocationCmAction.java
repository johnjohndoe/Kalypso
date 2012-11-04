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
package org.kalypso.model.wspm.pdb.ui.internal.tin.imports;

import org.eclipse.jface.action.IAction;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.gml.ui.coverage.CoverageManagementAction;
import org.kalypso.gml.ui.coverage.CoverageManagementWidget;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.PdbSettings;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * Imports a dhm from the cross section database.
 * 
 * @author Holger Albert
 */
public class ImportFromExternalLocationCmAction extends CoverageManagementAction
{
  private Shell m_shell;

  private CoverageManagementWidget m_widget;

  public ImportFromExternalLocationCmAction( )
  {
    super( Messages.getString( "ImportFromExternalLocationCmAction_0" ) ); //$NON-NLS-1$

    m_widget = null;
  }

  @Override
  public boolean isVisible( )
  {
    try
    {
      /* if no coverage can be added, we also cannot add via this method */
      if( !m_widget.isShowAddRemoveButton() )
        return false;

      // TODO: should this not be decided inside the action instead?

      final IPdbSettings[] settings = PdbSettings.getSettings();
      if( settings.length == 0 )
        return false;

      return true;
    }
    catch( final PdbConnectException ex )
    {
      // ex.printStackTrace();
      return false;
    }
  }

  @Override
  public void init( final Shell shell, final Object data )
  {
    m_shell = shell;
    m_widget = (CoverageManagementWidget)data;
  }

  @Override
  public IAction getAction( )
  {
    return new ImportFromExternalLocationAction( m_shell, m_widget );
  }
}
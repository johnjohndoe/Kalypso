/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.ui.view.table.swt.handlers;

import java.util.LinkedList;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointAdd;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.table.swt.ProfilSWTTableView;

/**
 * @author kimwerner
 */
public class AddPointHandler extends AbstractSWTTableHandler
{

  /**
   * @see org.kalypso.model.wspm.ui.view.table.swt.handlers.AbstractSWTTableHandler#doAction(java.util.LinkedList,
   *      org.kalypso.model.wspm.ui.view.table.swt.ProfilSWTTableView)
   */

  @Override
  public final IStatus executeEvent( LinkedList<IProfilPoint> selection, ProfilSWTTableView tableView )
  {
    if( tableView.isSorted() )
    {

      MessageDialog.openWarning( tableView.getControl().getShell(), "", "Punkte können nur in unsortierte Tabelle eingefügt werden.\nKlicken Sie auf die markierte Spalte, um die Sortierung aufzuheben." );
      return null;
    }

    final IProfilPoint thePointBefore = selection.isEmpty() ? null : selection.getLast();
    final IProfilPoint thePointAfter = thePointBefore == null ? null : ProfilUtil.getPointAfter( tableView.getProfil(), thePointBefore );
    IProfilPoint thePoint = thePointAfter == null ? tableView.getProfil().createProfilPoint() : ProfilUtil.splitSegment( tableView.getProfil(), thePointBefore, thePointAfter );
    final IProfilChange[] changes = new IProfilChange[2];
    changes[0] = new PointAdd( tableView.getProfil(), thePointBefore, thePoint );
    changes[1] = new ActiveObjectEdit( tableView.getProfil(), thePoint, IWspmConstants.POINT_PROPERTY_BREITE );
    final ProfilOperation operation = new ProfilOperation( "", tableView.getProfilEventManager(), changes, true );
    new ProfilOperationJob( operation ).schedule();

    return Status.OK_STATUS;
  }
}

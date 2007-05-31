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
package org.kalypso.model.wspm.ui.view.table.swt.handlers;

import java.util.LinkedList;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.contribs.eclipse.swt.custom.TableCursor;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.table.swt.ProfilCellModifier;
import org.kalypso.model.wspm.ui.view.table.swt.ProfilSWTTableView;

/**
 * @author kimwerner
 */
public class FillValuesHandler extends AbstractSWTTableHandler
{

  /**
   * @see org.kalypso.model.wspm.ui.view.table.swt.handlers.AbstractSWTTableHandler#doAction(java.util.LinkedList,
   *      org.kalypso.model.wspm.ui.view.table.swt.ProfilSWTTableView)
   */
  @Override
  public IStatus executeEvent( LinkedList<IProfilPoint> selection, ProfilSWTTableView tableView )
  {
    final TableCursor cursor = tableView.getCursor();
    final TableItem row = cursor.getRow();
    final int column = cursor.getColumn();

    final ProfilCellModifier cellModifier = tableView.getCellModifier();
    final String activeProp = cellModifier.getColumnProperty( column );
    final Double value = cellModifier.getValueAsDouble( row, activeProp );

    final IProfilChange[] changes = new IProfilChange[selection.size()];
    int i = 0;
    for( final IProfilPoint point : selection )
    {
      changes[i++] = new PointPropertyEdit( point, activeProp, value );
    }
    final ProfilOperation operation = new ProfilOperation( "Werte setzen", tableView.getProfilEventManager(), changes, false );
    new ProfilOperationJob( operation ).schedule();
    return Status.OK_STATUS;
  }
}

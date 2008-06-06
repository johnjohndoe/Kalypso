/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ogc.gml.om.table.command;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.kalypso.i18n.Messages;
import org.kalypso.observation.result.TupleResult;

/**
 * Deletes the selected lines from the table.
 * 
 * @author Gernot Belger
 */
public class DeleteSelectedRowsHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final TableViewer viewer = TupleResultCommandUtils.findTableViewer( event );
    final TupleResult tupleResult = TupleResultCommandUtils.findTupleResult( event );
    if( tupleResult == null || viewer == null )
      throw new ExecutionException( Messages.getString( "org.kalypso.ogc.gml.om.table.command.DeleteSelectedRowsHandler.0" ) ); //$NON-NLS-1$

    final IStructuredSelection selection = (IStructuredSelection) viewer.getSelection();

    Object firstElement = selection.getFirstElement();
    int firstIndex = tupleResult.indexOf( firstElement );

    // TODO: we should force the table to stop editing first

    tupleResult.removeAll( selection.toList() );

    if( tupleResult.size() > 0 )
    {
      int indexToSelect = Math.min( firstIndex, tupleResult.size() - 1 );
      viewer.setSelection( new StructuredSelection( tupleResult.get( indexToSelect ) ) );
    }

    return null;
  }
}

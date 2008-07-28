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
 * Moves the selected lines from the table one up.
 * 
 * @author kimwerner
 */
public class MoveUpSelectedRowsHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @SuppressWarnings("unchecked") //$NON-NLS-1$
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final TableViewer viewer = TupleResultCommandUtils.findTableViewer( event );
    final TupleResult tupleResult = TupleResultCommandUtils.findTupleResult( event );
    if( tupleResult == null || viewer == null )
      throw new ExecutionException( Messages.getString("org.kalypso.ogc.gml.om.table.command.MoveUpSelectedRowsHandler.1") ); //$NON-NLS-1$

    final IStructuredSelection selection = (IStructuredSelection) viewer.getSelection();

   
    final Object firstElement = selection.getFirstElement() ;
    final int index = tupleResult.indexOf(firstElement );
    if( index > 0 )
    {
      final int newPosition = index - 1;
      tupleResult.removeAll( selection.toList() );
      tupleResult.addAll( newPosition, selection.toList() );

    viewer.setSelection(new StructuredSelection(tupleResult.get( newPosition )));
    viewer.refresh();
    }
   
    return null;
  }

}

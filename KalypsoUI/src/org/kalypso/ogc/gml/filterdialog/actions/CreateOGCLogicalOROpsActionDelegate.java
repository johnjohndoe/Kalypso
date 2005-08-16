/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml.filterdialog.actions;

import java.util.ArrayList;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IActionDelegate;
import org.kalypso.ogc.gml.filterdialog.dialog.TreeSelection;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.LogicalOperation;
import org.kalypsodeegree_impl.filterencoding.OperationDefines;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author kuepfer
 */
public class CreateOGCLogicalOROpsActionDelegate implements IActionDelegate
{

  private IStructuredSelection m_selection;

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    if( m_selection != null && action.isEnabled() )
    {
      if( m_selection instanceof TreeSelection )
      {
        Object firstElement = m_selection.getFirstElement();
        if( firstElement instanceof ComplexFilter )
        {
          ComplexFilter filter = (ComplexFilter)firstElement;
          filter.setOperation( new LogicalOperation( OperationDefines.OR, new ArrayList() ) );
        }
        if( firstElement instanceof LogicalOperation )
        {
          LogicalOperation operation = (LogicalOperation)firstElement;
          //add new Logical Operation
          ArrayList arguments = operation.getArguments();
          if( arguments == null )
            arguments = new ArrayList();
          arguments.add( new LogicalOperation( OperationDefines.OR, new ArrayList() ) );
        }
        ( (TreeSelection)m_selection ).structureChanged();
      }
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    action.setEnabled( false );
    if( selection instanceof IStructuredSelection )
    {
      m_selection = (IStructuredSelection)selection;
      Object firstElement = m_selection.getFirstElement();
      Operation operation = null;
      if( firstElement instanceof ComplexFilter )
      {
        operation = ( (ComplexFilter)firstElement ).getOperation();
        if( operation == null )
          action.setEnabled( true );
      }
      if( firstElement instanceof LogicalOperation )
      {
        operation = (LogicalOperation)firstElement;
        int opsId = operation.getOperatorId();
        ArrayList arguments = ( (LogicalOperation)operation ).getArguments();
        if( arguments == null || arguments.size() < 2
            && ( opsId == OperationDefines.AND || opsId == OperationDefines.OR ) )
          action.setEnabled( true );
        if( arguments == null || arguments.size() < 1 && opsId == OperationDefines.NOT )
          action.setEnabled( true );
      }
    }
  }

}

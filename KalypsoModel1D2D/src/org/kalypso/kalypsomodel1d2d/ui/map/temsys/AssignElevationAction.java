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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Event;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;

/**
 * Assigns the elevation to all selected nodes.
 * 
 * @author Gernot
 */
class AssignElevationAction extends Action
{
  private final TableViewer m_nodeViewer;

  private final ApplyElevationWidgetDataModel m_dataModel;

  public AssignElevationAction( final TableViewer nodeViewer, final ApplyElevationWidgetDataModel dataModel )
  {
    m_nodeViewer = nodeViewer;
    m_dataModel = dataModel;

    setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.26" ) ); //$NON-NLS-1$
  }

  @Override
  public void runWithEvent( final Event event )
  {
    applyElevation();
  }

  protected final void applyElevation( )
  {
    try
    {
      final ISelection selection = m_nodeViewer.getSelection();
      if( !(selection instanceof IStructuredSelection) )
        return;

      final List<IFE1D2DNode> nodeList = new ArrayList<>();

      for( final Object selected : ((IStructuredSelection)selection).toList() )
      {
        if( selected instanceof IFE1D2DNode )
          nodeList.add( (IFE1D2DNode)selected );
      }

      ApplyElevationHelper.assignElevationToSelectedNodes( m_dataModel, nodeList );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    m_nodeViewer.refresh();
  }
}
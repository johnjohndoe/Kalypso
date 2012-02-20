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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
class ElevationModelMoveSelectionAction extends Action
{
  private final TableViewer m_elevationViewer;

  private final ApplyElevationWidgetDataModel m_dataModel;

  private final int m_direction;

  ElevationModelMoveSelectionAction( final TableViewer elevationViewer, final ApplyElevationWidgetDataModel dataModel, final int direction )
  {
    m_elevationViewer = elevationViewer;
    m_dataModel = dataModel;
    m_direction = direction;
  }

  @Override
  public void run( )
  {
    final ISelection selection = m_elevationViewer.getSelection();
    if( selection instanceof IStructuredSelection )
    {
      final Object firstElement = ((IStructuredSelection) selection).getFirstElement();
      if( firstElement instanceof ITerrainElevationModel )
      {
        final IFeatureBindingCollection<ITerrainElevationModel> elevationModels = m_dataModel.getTerrainElevationModels();
        if( elevationModels == null )
          return;

        final int i = elevationModels.indexOf( firstElement );
        final int targetPos = i + m_direction;
        final int SIZE = elevationModels.size();

        if( i < 0 || targetPos < 0 || targetPos >= SIZE )
        {
          // not found
          return;
        }
        else
        {
          final ITerrainElevationModel modelToReplace = elevationModels.get( targetPos );
          elevationModels.set( targetPos, (ITerrainElevationModel) firstElement );
          elevationModels.set( i, modelToReplace );
        }
      }
    }

    m_elevationViewer.refresh();
  }
}

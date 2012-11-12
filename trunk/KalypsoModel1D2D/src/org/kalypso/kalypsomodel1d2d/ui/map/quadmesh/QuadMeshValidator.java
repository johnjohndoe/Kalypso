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
package org.kalypso.kalypsomodel1d2d.ui.map.quadmesh;

import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * Validates a QuadMesh within itself and against a discretisation model.
 * 
 * @author Gernot Belger
 */
public class QuadMeshValidator
{
  private final QuadMesh m_mesh;

  public QuadMeshValidator( final QuadMesh mesh )
  {
    m_mesh = mesh;
  }

  public IStatus isValid( final IFEDiscretisationModel1d2d discModel )
  {
    try
    {
      if( m_mesh == null )
        return Status.OK_STATUS;

      return checkElements( discModel );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "QuadMeshValidator.0" ), e ); //$NON-NLS-1$
    }
  }

  private IStatus checkElements( final IFEDiscretisationModel1d2d discModel ) throws GM_Exception
  {
    if( discModel == null )
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.util.TempGrid.4" ) ); //$NON-NLS-1$

    final List<GM_PolygonPatch> rings = m_mesh.toRings();
    for( final GM_PolygonPatch ring : rings )
    {
      // 4) New Element self-intersects
      if( GeometryUtilities.isSelfIntersecting( ring.getExteriorRing() ) )
        return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.util.TempGrid.5" ) ); //$NON-NLS-1$

      // New Element intersects other elements
      final GM_Polygon newSurface = GeometryFactory.createGM_Surface( ring );
      final List<IFE1D2DElement> elements = discModel.queryElements( newSurface.getEnvelope(), null );
      for( final IFE1D2DElement element : elements )
      {
        if( element instanceof IPolyElement )
        {
          final GM_Polygon eleGeom = ((IPolyElement)element).getGeometry();
          if( eleGeom.intersects( newSurface ) )
          {
            final GM_Object intersection = eleGeom.intersection( newSurface );
            if( intersection instanceof GM_Polygon )
              return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.util.TempGrid.6" ) ); //$NON-NLS-1$
          }
        }
      }
    }

    return Status.OK_STATUS;
  }
}

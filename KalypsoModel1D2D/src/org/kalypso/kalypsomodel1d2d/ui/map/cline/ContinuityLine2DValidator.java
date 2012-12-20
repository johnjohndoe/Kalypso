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
package org.kalypso.kalypsomodel1d2d.ui.map.cline;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.LineString;

/**
 * Validates nodes as 2D continuity line
 * 
 * @author Gernot Belger
 */
class ContinuityLine2DValidator
{
  private final IMapPanel m_panel;

  private final IFE1D2DNode[] m_nodes;

  public ContinuityLine2DValidator( final IMapPanel panel, final IFE1D2DNode[] nodes )
  {
    m_panel = panel;
    m_nodes = nodes;
  }

  public String execute( )
  {
    try
    {
      if( m_nodes == null )
        return "Unable to build a connected continuity line";

      if( m_nodes.length < 2 )
        return Messages.getString( "ContinuityLine2DValidator_0" ); //$NON-NLS-1$

      /* build geometry */
      final LineGeometryBuilder lineBuilder = CreateFEContinuityLineWidget.createLineBuilder( m_panel, m_nodes, null );
      final GM_Curve curve = (GM_Curve)lineBuilder.finish();
      if( curve == null )
        return null;

      /* self intersection */
      final LineString line = (LineString)JTSAdapter.export( curve );
      if( !line.isSimple() )
        return Messages.getString( "ContinuityLine2DValidator_1" ); //$NON-NLS-1$

      // REMARK: 2d lines may intersect and touch!

//      /* intersection with other conti lines */
//      // REMARK: we assume that we have not too many conti lines and just do a linear search here
//      final IFELine[] contiLines = m_discModel.getContinuityLines();
//      for( final IFELine contiLine : contiLines )
//      {
//        final GM_Curve geometry = contiLine.getGeometry();
//        if( curve.intersects( geometry ) )
//          return "Line intersect an existing continuity line";
//      }

      return null;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return e.getLocalizedMessage();
    }
  }
}

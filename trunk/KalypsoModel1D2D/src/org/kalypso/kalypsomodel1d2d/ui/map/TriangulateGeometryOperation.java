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
package org.kalypso.kalypsomodel1d2d.ui.map;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.util.Add2DElementsCommand;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;

/**
 * @author Gernot Belger
 */
public class TriangulateGeometryOperation
{
  private final TriangulateGeometryWidget m_widget;

  public TriangulateGeometryOperation( final TriangulateGeometryWidget widget )
  {
    m_widget = widget;
  }

  // FIXME: move into operation
  public void convertTriangulationToModel( )
  {
    final IKalypsoFeatureTheme theme = m_widget.getDiscTheme();
    final CommandableWorkspace workspace = theme.getWorkspace();

    try
    {
      final List<GM_PolygonPatch> elements = getTinRings();
      if( elements == null )
        return;

      final Add2DElementsCommand command = new Add2DElementsCommand( workspace, elements );
      workspace.postCommand( command );
    }
    catch( final Exception e1 )
    {
      e1.printStackTrace();
      SWT_AWT_Utilities.showSwtMessageBoxError( m_widget.getName(), Messages.getString( "TriangulateGeometryData_0" ) + e1.toString() ); //$NON-NLS-1$
    }
    finally
    {
      m_widget.reinit();
    }
  }

  private List<GM_PolygonPatch> getTinRings( )
  {
    final GM_TriangulatedSurface tin = m_widget.getBuilder().getTin();
    if( tin == null )
      return null;

    final List<GM_PolygonPatch> rings = new ArrayList<>( tin.size() );
    rings.addAll( tin );
    return rings;
  }
}
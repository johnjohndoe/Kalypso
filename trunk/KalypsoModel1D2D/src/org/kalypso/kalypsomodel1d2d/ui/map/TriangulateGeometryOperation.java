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

import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.dikeditchgen.TriangulationBuilder;
import org.kalypso.kalypsomodel1d2d.ui.map.util.Add2DElementsCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;

/**
 * @author Gernot Belger
 */
public class TriangulateGeometryOperation
{
  private final TriangulationBuilder m_builder;

  private final IFEDiscretisationModel1d2d m_discretizationModel;

  public TriangulateGeometryOperation( final TriangulationBuilder triangulationBuilder, final IFEDiscretisationModel1d2d discretizationModel )
  {
    m_builder = triangulationBuilder;
    m_discretizationModel = discretizationModel;
  }

  public void convertTriangulationToModel( )
  {
    final CommandableWorkspace workspace = new CommandableWorkspace( m_discretizationModel.getWorkspace() );

    try
    {
      m_builder.finish();

      final GM_TriangulatedSurface tin = m_builder.getTin();
      if( tin == null )
        return;

      final Add2DElementsCommand command = new Add2DElementsCommand( workspace, tin );
      workspace.postCommand( command );
    }
    catch( final Exception e1 )
    {
      e1.printStackTrace();
      SWT_AWT_Utilities.showSwtMessageBoxError( Messages.getString( "TriangulateGeometryData_0" ), e1.toString() ); //$NON-NLS-1$
    }
  }

}
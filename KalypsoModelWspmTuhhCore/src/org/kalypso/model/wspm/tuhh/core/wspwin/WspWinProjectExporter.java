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
package org.kalypso.model.wspm.tuhh.core.wspwin;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IProject;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.wspwin.core.WspCfg.TYPE;

/**
 * Exports a water body and some of it's reaches as a wspwin project.
 *
 * @author Gernot Belger
 */
public class WspWinProjectExporter
{
  private final TuhhReach[] m_reaches;

  private final TYPE m_projectType;

  private final WspmWaterBody m_waterBody;

  private final String m_roughnessType;

  private final boolean m_preferRoughnessClasses;

  private final boolean m_preferVegetationClasses;

  public WspWinProjectExporter( final WspmWaterBody waterBody, final TuhhReach[] reaches, final TYPE projectType, final String roughnessType, final boolean preferRoughnessClasses, final boolean preferVegetationClasses )
  {
    m_waterBody = waterBody;
    m_reaches = reaches;
    m_projectType = projectType;
    m_roughnessType = roughnessType;
    m_preferRoughnessClasses = preferRoughnessClasses;
    m_preferVegetationClasses = preferVegetationClasses;
  }

  public void export( final File outputDir ) throws IOException
  {
    outputDir.mkdirs();

    final URL context = m_waterBody.getWorkspace().getContext();
    final IProject project = ResourceUtilities.findProjectFromURL( context );
    final String probez = project == null ? StringUtils.EMPTY : project.getName();

    final WspWinProjectWriter wspWinProjectWriter = new WspWinProjectWriter( probez, m_projectType, outputDir, m_roughnessType, m_preferRoughnessClasses, m_preferVegetationClasses );

    wspWinProjectWriter.addReaches( m_waterBody, m_reaches );

    wspWinProjectWriter.write();
  }
}
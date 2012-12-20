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
import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.wspwin.core.WspCfg.TYPE;

/**
 * @author Gernot Belger
 */
public class WspWinExportGmlOperation implements ICoreRunnableWithProgress
{
  private final WspWinExportGmlData m_data;

  public WspWinExportGmlOperation( final WspWinExportGmlData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    try
    {
      final WspmWaterBody[] waters = m_data.getWaterBodies();
      monitor.beginTask( Messages.getString( "WspWinExportGmlOperation_0" ), waters.length ); //$NON-NLS-1$

      final File outputDir = m_data.getOutputDir();

      if( outputDir.isDirectory() )
        FileUtils.cleanDirectory( outputDir );

      final TYPE projectType = m_data.getProjectType();

      final String probez = getProjectName( waters );

      final String roughnessType = m_data.getRoughnessType();
      final boolean preferRoghnessClasses = m_data.getPreferRoughnessClasses();
      final boolean preferVegetationClasses = m_data.getPreferVegetationClasses();

      final WspWinProjectWriter wspWinWriter = new WspWinProjectWriter( probez, projectType, outputDir, roughnessType, preferRoghnessClasses, preferVegetationClasses );

      for( final WspmWaterBody waterBody : waters )
      {
        monitor.subTask( waterBody.getName() );

        final TuhhReach[] reaches = m_data.getReaches( waterBody );

        wspWinWriter.addReaches( waterBody, reaches );
      }

      wspWinWriter.write();

      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new InvocationTargetException( e );
    }
    finally
    {
      monitor.done();
    }
  }

  private String getProjectName( final WspmWaterBody[] waters )
  {
    if( waters.length == 0 )
      return StringUtils.EMPTY;

    final URL context = waters[0].getWorkspace().getContext();
    final IProject project = ResourceUtilities.findProjectFromURL( context );
    if( project == null )
      return StringUtils.EMPTY;

    return project.getName();
  }
}
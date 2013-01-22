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
package org.kalypso.model.wspm.tuhh.ui.imports.sobek;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.sobek.SobekModel;
import org.kalypso.model.wspm.core.profil.sobek.parser.SobekModelParser;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author Gernot Belger
 */
public class SobekImportOperation implements ICoreRunnableWithProgress
{
  private final SobekImportData m_data;

  public SobekImportOperation( final SobekImportData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    monitor.beginTask( Messages.getString( "SobekImportOperation.0" ), 100 ); //$NON-NLS-1$

    try
    {
      // find files for parsing
      final FileAndHistoryData inputDir = m_data.getInputDir();
      final SobekModelParser modelParser = new SobekModelParser( inputDir.getFile(), m_data.getSrs() );
      final SobekModel model = modelParser.read( new SubProgressMonitor( monitor, 30 ) );

      final Sobek2Wspm sobek2Wspm = new Sobek2Wspm( m_data );
      sobek2Wspm.convert( model );

      final CommandableWorkspace workspace = m_data.getWorkspace();
      final WspmWaterBody water = m_data.getWater();
      final Feature[] newFeatures = sobek2Wspm.getNewFeatures();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, water, newFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

      workspace.postCommand( new EmptyCommand( StringUtils.EMPTY, false ) );

      return sobek2Wspm.getStatus();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new InvocationTargetException( e );
    }
    catch( final CoreException e )
    {
      throw e;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new InvocationTargetException( e );
    }
  }
}
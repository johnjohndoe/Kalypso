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
package org.kalypso.model.wspm.tuhh.ui.actions;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.simulation.ui.calccase.ModelNature;

/**
 * The calc tuhh job.
 *
 * @author Holger Albert
 */
public class CalcTuhhJob extends Job
{
  /**
   * The calculation.
   */
  private final TuhhCalculation m_calculation;

  /**
   * The gml file.
   */
  private final IFile m_gmlFile;

  /**
   * The constructor.
   *
   * @param calculation
   *          The calculation.
   * @param gmlFile
   *          The gml file.
   */
  public CalcTuhhJob( final String name, final TuhhCalculation calculation, final IFile gmlFile )
  {
    super( name );

    m_calculation = calculation;
    m_gmlFile = gmlFile;
  }

  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    try
    {
      final IProject project = m_gmlFile.getProject();
      final String natureId = ModelNature.ID;
      final ModelNature nature = (ModelNature) project.getNature( natureId );
      if( nature == null )
      {
        final String message = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhHandler.5" ) + ModelNature.ID; //$NON-NLS-1$
        return new Status( IStatus.WARNING, KalypsoModelWspmTuhhUIPlugin.getID(), message );
      }

      final String calcxpath = String.format( "id('%s')", m_calculation.getId() ); //$NON-NLS-1$
      final String resultPath = m_calculation.getResultFolder().toPortableString();

      final Map<String, Object> properties = new HashMap<>();
      properties.put( "calc.xpath", calcxpath ); //$NON-NLS-1$
      properties.put( "result.path", resultPath ); //$NON-NLS-1$

      return nature.launchAnt( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhHandler.8" ), "calc", properties, m_gmlFile.getParent(), monitor ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    catch( final OperationCanceledException e )
    {
      return Status.CANCEL_STATUS;
    }
    catch( final Throwable t )
    {
      return StatusUtilities.statusFromThrowable( t );
    }
  }
}
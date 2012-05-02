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
package org.kalypso.ui.rrm.internal.simulations.jobs;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus;
import org.kalypsodeegree_impl.gml.binding.commons.StatusCollection;

/**
 * This job reads the calculation status.
 * 
 * @author Holger Albert
 */
public class ReadCalculationStatusJob extends Job
{
  /**
   * The simulation.
   */
  private final RrmSimulation m_simulation;

  /**
   * The calculation status.
   */
  private IStatus m_calculationStatus;

  /**
   * The constructor.
   * 
   * @param simulation
   *          The simulation.
   */
  public ReadCalculationStatusJob( final RrmSimulation simulation )
  {
    super( "ReadCalculationStatusJob" );

    m_simulation = simulation;
    m_calculationStatus = new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "Not available." );
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( IProgressMonitor monitor )
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    try
    {
      /* Monitor. */
      monitor.beginTask( "Reading the calculation status...", 1000 );
      monitor.subTask( "Reading the calculation status..." );

      /* Read the calculation status. */
      m_calculationStatus = readCalculationStatus();

      /* Monitor. */
      monitor.worked( 1000 );

      return Status.OK_STATUS;
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  /**
   * This function returns the calculation status.
   * 
   * @return The calculation status.
   */
  public IStatus getCalculationStatus( )
  {
    return m_calculationStatus;
  }

  /**
   * This function reads the calculation status and returns it.
   * 
   * @return The calculation status.
   */
  private IStatus readCalculationStatus( )
  {
    try
    {
      /* Memory for the results. */
      final StatusCollector results = new StatusCollector( KalypsoUIRRMPlugin.getID() );

      /* Get the file of the calculation status gml. */
      final IFile calculationStatusGml = m_simulation.getCalculationStatusGml();

      /* Load the workspace. */
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( calculationStatusGml );

      /* Get the root feature. */
      final Feature rootFeature = workspace.getRootFeature();

      /* Cast to status collection. */
      final StatusCollection statusCollection = (StatusCollection) rootFeature;

      /* Add all geo status objects to the list of the results. */
      for( final IGeoStatus geoStatus : statusCollection.getStati() )
        results.add( geoStatus );

      return results.asMultiStatusOrOK( "The calculation was not executed.", "The calculation was executed." );
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Reading the calculation status has failed.", ex );
    }
  }
}
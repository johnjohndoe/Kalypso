/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.convert.namodel;

import java.io.File;
import java.net.URL;
import java.util.logging.Logger;

import org.kalypso.convert.namodel.optimize.NAOptimizingJob;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.optimize.IOptimizingJob;
import org.kalypso.optimize.OptimizerCalJob;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming
 */
public class NaModelCalcJob implements ICalcJob
{

  private ICalcJob m_calcJob = null;

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( File tmpdir, ICalcDataProvider dataProvider, ICalcResultEater resultEater, final ICalcMonitor monitor )
      throws CalcJobServiceException
  {
    try
    {
      final Logger logger = Logger.getAnonymousLogger();
      // testen ob calcjob optimization hat
      //      final URL schemaURL = getClass().getResource( "schema/nacontrol.xsd" );
      final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( dataProvider
          .getURLForID( NaModelConstants.IN_CONTROL_ID ) );
      final Feature rootFeature = controlWorkspace.getRootFeature();
      final boolean optimize = FeatureHelper.booleanIsTrue( rootFeature, "automaticCallibration", false );

      if( optimize )
      {
        final IOptimizingJob optimizeJob;
        optimizeJob = new NAOptimizingJob( tmpdir, dataProvider, monitor );
        m_calcJob = new OptimizerCalJob( logger, optimizeJob );
      }
      else
        m_calcJob = new NaModelInnerCalcJob();

      if( m_calcJob != null )
        m_calcJob.run( tmpdir, dataProvider, resultEater, monitor );
    }
    catch( Exception e )
    {
      throw new CalcJobServiceException( "could not instantiate NAOptimizingJob", e );
    }
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
  {
    return getClass().getResource( "resources/nacalcjob_spec.xml" );
  }
}
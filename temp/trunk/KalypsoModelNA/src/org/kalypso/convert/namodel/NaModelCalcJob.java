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
import java.io.FileWriter;
import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.kalypso.convert.namodel.optimize.NAOptimizingJob;
import org.kalypso.convert.namodel.optimize.OptimizeCalcDataProvider;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.optimize.IOptimizingJob;
import org.kalypso.optimize.OptimizerCalJob;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author doemming
 */
public class NaModelCalcJob implements ICalcJob
{

  private ICalcJob m_calcJob = null;

  private static final String MEASURE_SEAL_FEATURE = "MeasureSealing";

  private static final String MEASURE_PROP_SEAL_GEOM = "areaOfInterest";

  private static final String MEASURE_PROP_SEAL_FACTOR = "grz";

  private static final String HYDRO_PROP_GEOM = "Ort";

  private static final String HYDRO_PROP_SEAL_FACTOR = "m_vers";

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( final File tmpdir, final ICalcDataProvider dataProvider, final ICalcResultEater resultEater,
      final ICalcMonitor monitor ) throws CalcJobServiceException
  {
    try
    {
      final Logger logger = Logger.getAnonymousLogger();

      // TODO make seperate calcjob for next function !
      final ICalcDataProvider innerDataProvider = getDataProviderFromMeasure( logger, dataProvider, tmpdir );

      // testen ob calcjob optimization hat
      //      final URL schemaURL = getClass().getResource( "schema/nacontrol.xsd" );
      final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( innerDataProvider
          .getURLForID( NaModelConstants.IN_CONTROL_ID ) );
      final Feature rootFeature = controlWorkspace.getRootFeature();
      final boolean optimize = FeatureHelper.booleanIsTrue( rootFeature, "automaticCallibration", false );

      if( optimize )
      {
        final IOptimizingJob optimizeJob;
        optimizeJob = new NAOptimizingJob( tmpdir, innerDataProvider, monitor );
        m_calcJob = new OptimizerCalJob( logger, optimizeJob );
      }
      else
        m_calcJob = new NaModelInnerCalcJob();

      if( m_calcJob != null )
        m_calcJob.run( tmpdir, innerDataProvider, resultEater, monitor );
    }
    catch( Exception e )
    {
      throw new CalcJobServiceException( "could not instantiate NAOptimizingJob", e );
    }
  }

  /**
   * @param originalDataProvider
   * @param tmpDir
   *          dir for temporary files
   * @return modified dataprovider including measures or same dataprovider if no measures are used <br>
   *         TODO move this business to a measureclacjob !
   */
  private ICalcDataProvider getDataProviderFromMeasure( final Logger logger,
      final ICalcDataProvider originalDataProvider, final File tmpDir )
  {
    final OptimizeCalcDataProvider result = new OptimizeCalcDataProvider( originalDataProvider );
    if( !( originalDataProvider.hasID( NaModelConstants.IN_MEASURE_ID ) && originalDataProvider
        .hasID( NaModelConstants.IN_HYDROTOP_ID ) ) )
    {
      final StringBuffer buffer = new StringBuffer();
      if( !originalDataProvider.hasID( NaModelConstants.IN_MEASURE_ID ) )
        buffer.append( "optional dataset 'measures' is not provided, " );
      if( !originalDataProvider.hasID( NaModelConstants.IN_HYDROTOP_ID ) )
        buffer.append( "dataset 'hydrotope' is not provided, " );
      buffer.append( "so no measures will be used..." );
      logger.info( buffer.toString() );
      return result;
    }
    try
    {
      logger.info( "start using dataset 'measure' for updating hydrotops ..." );
      final URL measureURL = originalDataProvider.getURLForID( NaModelConstants.IN_MEASURE_ID );
      final URL hydrotopURL = originalDataProvider.getURLForID( NaModelConstants.IN_HYDROTOP_ID );
      final GMLWorkspace measureWorkspace = GmlSerializer.createGMLWorkspace( measureURL );
      final FeatureType sealingFT = measureWorkspace.getFeatureType( MEASURE_SEAL_FEATURE );
      final Feature[] sealingFEs = measureWorkspace.getFeatures( sealingFT );
      if( sealingFEs.length == 0 )
      {
        logger.info( "measure is empty, continue normal simulation (withouit measures)" );
        return result;
      }
      final GMLWorkspace hydroWorkspace = GmlSerializer.createGMLWorkspace( hydrotopURL );
      //      final FeatureType hydroFT = hydroWorkspace.getFeatureType( null );
      final FeatureList hydroList = (FeatureList)hydroWorkspace
          .getFeatureFromPath( "HydrotopCollectionMember/HydrotopMember" );

      // for geometry operations hydroworkspace and measureworkspace must use the same coordinatessystem,
      // so let measure transform to the one hydo uses. (less work than other way)
      if( hydroList.size() > 0 )
      {
        final Feature feature = (Feature)hydroList.get( 0 );
        final GM_Object geom = feature.getGeometryProperties()[0];
        final CS_CoordinateSystem targetCS = geom.getCoordinateSystem();
        final TransformVisitor visitor = new TransformVisitor( targetCS );
        measureWorkspace.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE );
      }

      for( int i = 0; i < sealingFEs.length; i++ )
      {
        final Feature sealFE = sealingFEs[i];
        double sealMeasure = FeatureHelper.getAsDouble( sealFE, MEASURE_PROP_SEAL_FACTOR, 1.0d );
        final GM_Object measureGEOM = (GM_Object)sealFE.getProperty( MEASURE_PROP_SEAL_GEOM );
        final Geometry jtsMeasureGEOM = JTSAdapter.export( measureGEOM );
        //        final double areaMeasure = jtsMeasureGEOM.getArea();
        final GM_Envelope selENV = sealFE.getEnvelope();
        final List hydrosInENV = hydroList.query( selENV, null );
        for( Iterator iter = hydrosInENV.iterator(); iter.hasNext(); )
        {
          final Feature hydroFE = (Feature)iter.next();
          final double sealHydro = FeatureHelper.getAsDouble( hydroFE, HYDRO_PROP_SEAL_FACTOR, 1.0d );

          final GM_Object hydroGEOM = (GM_Object)hydroFE.getProperty( HYDRO_PROP_GEOM );
          final Geometry jtsHydroGEOM = JTSAdapter.export( hydroGEOM );
          Geometry intersection = null;
          try
          {
            intersection = jtsMeasureGEOM.intersection( jtsHydroGEOM );
          }
          catch( Exception e )
          {
            // nothing
          }
          if( intersection == null || intersection.isEmpty() )
            continue;
          final double areaIntersection = intersection.getArea();
          final double areaHydro = jtsHydroGEOM.getArea();
          // TODO check for numerical best order
          // remark: it does not matter, if in next loop the same hydrotop again is affected
          final double newSealFactor = ( ( areaHydro - areaIntersection ) * sealHydro + areaIntersection * sealMeasure )
              / areaHydro;
          hydroFE.setProperty( HYDRO_PROP_SEAL_FACTOR, new Double( newSealFactor ) );
        }
      }
      final File hydroTopFile = File.createTempFile( "measured_hydrotops", "gml", tmpDir );
      final FileWriter writer = new FileWriter( hydroTopFile );
      try
      {
        GmlSerializer.serializeWorkspace( writer, hydroWorkspace, writer.getEncoding() );
      }
      finally
      {
        IOUtils.closeQuietly( writer );
      }
      final URL newHydroURL = hydroTopFile.toURL();
      // now overwrite the old hydros
      result.addURL( NaModelConstants.IN_HYDROTOP_ID, newHydroURL );
    }
    catch( Exception e )
    {
      logger.info( "could not read measure from dataprovider, will continue no meauser used,\n reason:"
          + e.getLocalizedMessage() );
      return result;
    }
    return result;
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
  {
    return getClass().getResource( "resources/nacalcjob_spec.xml" );
  }
}
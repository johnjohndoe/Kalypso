package org.kalypso.convert.namodel;

import java.io.File;
import java.net.URL;
import java.util.logging.Logger;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.model.feature.FeatureHelper;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.optimize.IOpmizingJob;
import org.kalypso.optimize.OptimizerCalJob;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.impl.CalcJobHelper;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author doemming
 */
public class NaModelCalcJob implements ICalcJob
{
  private final static Logger LOGGER = Logger.getLogger( NaModelCalcJob.class.getName() );

  private ICalcJob m_calcJob = null;

  public void run( File basedir, CalcJobDataBean[] beans ) throws CalcJobServiceException
  {
    try
    {
      // testen ob calcjob optimization hat
      final File baseInputDir = new File( basedir, ICalcServiceConstants.INPUT_DIR_NAME );
      final CalcJobDataBean controlBean = CalcJobHelper.getBeanForId( NaModelConstants.CONTROL_ID,
          beans );
      final File controlFile = new File( baseInputDir, controlBean.getPath() );
      final URL schemaURL = getClass().getResource( "schema/nacontrol.xsd" );
      final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlFile.toURL(),
          schemaURL );
      final Feature rootFeature = controlWorkspace.getRootFeature();
      final boolean optimize = FeatureHelper.booleanIsTrue( rootFeature, "automaticCallibration",
          false );
      
      if( optimize )
      {
        final IOpmizingJob optimizeJob;
        optimizeJob = new NAOptimizingJob( basedir, beans );
        m_calcJob = new OptimizerCalJob( LOGGER, optimizeJob );
      }
      else
        m_calcJob = new NaModelInnerCalcJob();

      if( m_calcJob != null )
        m_calcJob.run( basedir, beans );
    }
    catch( Exception e )
    {
      throw new CalcJobServiceException( "could not instantiate NAOptimizingJob", e );
    }
  }

  public void cancel()
  {
    if( m_calcJob != null )
      m_calcJob.cancel();
  }

  public void disposeJob()
  {
    if( m_calcJob != null )
      m_calcJob.disposeJob();
  }

  public String getMessage()
  {
    if( m_calcJob != null )
      return m_calcJob.getMessage();
    return "";
  }

  public int getProgress()
  {
    if( m_calcJob != null )
      return m_calcJob.getProgress();
    return 0;
  }

  public CalcJobDataBean[] getResults()
  {
    if( m_calcJob != null )
      return m_calcJob.getResults();
    return null;
  }

  public boolean isCanceled()
  {
    if( m_calcJob != null )
      return m_calcJob.isCanceled();
    return false;
  }

}
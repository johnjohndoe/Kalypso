package org.kalypso.optimize.test;

import java.io.File;
import java.util.logging.Logger;

import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;
import org.deegree_impl.extension.ITypeRegistry;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.kalypso.convert.namodel.NaModelCalcJob;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.service.CalcJobDataBean;

/**
 * @author doemming
 *  
 */
public class OptimizedNaModellTest extends TestCase
{
  private final static Logger LOGGER = Logger.getLogger( OptimizedNaModellTest.class.getName() );

  public void testRun() throws Exception
  {
    final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
    registry.registerTypeHandler( new ObservationLinkHandler() );

    final File baseDir = new File( "C:\\Programme\\KalypsoServer\\data\\tmp\\TEST" );
    final File simDir = new File( baseDir, ICalcServiceConstants.CALC_DIR_NAME );
    final File ergDir = new File( baseDir, ICalcServiceConstants.OUTPUT_DIR_NAME );
    if( simDir.exists() )
      FileUtils.cleanDirectory( simDir );
    if( ergDir.exists() )
      FileUtils.cleanDirectory( ergDir );

    final CalcJobDataBean[] beans = new CalcJobDataBean[]
    {
        new CalcJobDataBean( NaModelConstants.MODELL_ID, "Modelldaten", "calc/calcCase.gml" ),
        new CalcJobDataBean( NaModelConstants.CONTROL_ID, "Steuerdaten", "calc/.nacontrol.gml" ),
        new CalcJobDataBean( NaModelConstants.META_ID, "MetaSteuerdaten", "calc/.calculation" ),
        new CalcJobDataBean( "NiederschlagDir", "niederschlag", "calc/Niederschlag/" ),
        new CalcJobDataBean( "ZuflussDir", "zufluesse", "calc/Zufluss/" ),
        new CalcJobDataBean( "PegelDir", "pegel", "calc/Pegel/" ),
        new CalcJobDataBean( NaModelConstants.OPTIMIZECONF_ID, "optimizeConf", "calc/.sce.xml" ) };
    try
    {
      ICalcJob job = new NaModelCalcJob();
      //      final IOpmizingJob optimizeJob = new NAOptimizingJob( baseDir, beans );
      //      final ICalcJob job = new OptimizerCalJob( LOGGER, optimizeJob );
      job.run( baseDir, beans );
      CalcJobDataBean[] results = job.getResults();
      for( int i = 0; i < results.length; i++ )
      {
        CalcJobDataBean bean = results[i];
        LOGGER.info( bean.toString() );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }
}
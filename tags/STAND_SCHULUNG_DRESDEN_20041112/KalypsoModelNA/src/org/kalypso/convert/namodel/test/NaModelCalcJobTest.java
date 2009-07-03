package org.kalypso.convert.namodel.test;

import java.io.File;

import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;
import org.deegree_impl.extension.ITypeRegistry;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.kalypso.convert.namodel.NaModelCalcJob;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.services.calculation.service.CalcJobDataBean;

/**
 * @author doemming
 *  
 */
public class NaModelCalcJobTest extends TestCase
{
  
  public void testRun() throws Exception
  {
    final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
    registry.registerTypeHandler( new ObservationLinkHandler() );

    //    File baseDir = new File( "C:\\simulation\\test" );
    final File baseDir = new File( "C:\\Programme\\KalypsoServer\\data\\tmp\\TEST" );
    final File simDir = new File( baseDir, "sim" );
    final File ergDir = new File( baseDir, "output" );
    if( simDir.exists() )
      FileUtils.cleanDirectory( simDir ); 
    if( ergDir.exists() )
      FileUtils.cleanDirectory( ergDir );

    //    File baseDir = FileUtilities.createNewTempDir( "NA_Simulation" );
    //    baseDir.mkdirs();

    //    final File inputdir = new File( baseDir,
    // ICalcServiceConstants.INPUT_DIR_NAME );
    //    inputdir.mkdirs();
    //    final File modellGML = new File( inputdir, "calcCase.gml" );
    //    final File controlGML = new File( inputdir, "nacontrol.gml" );
    //
    //    StreamUtilities.streamCopy( getClass().getResourceAsStream(
    // modellGMLResource ),
    //        new FileOutputStream( modellGML ) );
    //    StreamUtilities.streamCopy( getClass().getResourceAsStream(
    // controlGMLResource ),
    //        new FileOutputStream( controlGML ) );

    final CalcJobDataBean[] beans = new CalcJobDataBean[]
    {
        new CalcJobDataBean( NaModelCalcJob.MODELL_ID, "Modelldaten", "calc/calcCase.gml" ),
        new CalcJobDataBean( NaModelCalcJob.CONTROL_ID, "Steuerdaten", "calc/.nacontrol.gml" ),
        new CalcJobDataBean( NaModelCalcJob.META_ID, "MetaSteuerdaten", "calc/.calculation" ),
        new CalcJobDataBean( "NiederschlagDir", "niederschlag", "calc/Niederschlag/" ),
        new CalcJobDataBean( "ZuflussDir", "zufluesse", "calc/Zufluss/" ),
        new CalcJobDataBean( "PegelDir", "pegel", "calc/Pegel/" ), };
    try
    {
      final NaModelCalcJob job = new NaModelCalcJob();
      job.run( baseDir, beans );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }
}
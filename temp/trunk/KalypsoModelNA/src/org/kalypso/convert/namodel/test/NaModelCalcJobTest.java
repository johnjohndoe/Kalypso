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
  //  final String modellGMLResource = "data/namodell.gml";
  //
  //  final String controlGMLResource = "data/nacontrol.gml";

  // TODO: andreas: hier ist Dein alter Test:
  //  public void testRun() throws Exception
  //  {
  //    // general
  //    final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
  //    registry.registerTypeHandler( new ObservationLinkHandler() );
  //
  //    // File modellGML = File.createTempFile( "NA_MODELL", ".gml" );
  //    // File controlGML = File.createTempFile( "NA_CONTROL", ".gml" );
  //    File modellGML = new File( "C:\\simulation\\namodell.gml" );
  //    File controlGML = new File( "C:\\simulation\\nacontrol.gml" );
  //
  //    // StreamUtilities.streamCopy( getClass().getResourceAsStream(
  //    // modellGMLResource ),
  //    // new FileOutputStream( modellGML ) );
  //    // StreamUtilities.streamCopy( getClass().getResourceAsStream(
  //    // controlGMLResource ),
  //    // new FileOutputStream( controlGML ) );
  //
  //    // File baseDir = FileUtilities.createNewTempDir( "NA_Simulation" );
  //
  //    NaModelCalcJob job = new NaModelCalcJob();
  //    CalcJobDataBean[] beans = new CalcJobDataBean[]
  //    {
  //        // new CalcJobDataBean("id", "name", "path"),
  //        new CalcJobDataBean( NaModelCalcJob.MODELL_ID, "Modelldaten",
  // modellGML.getPath() ),
  //        new CalcJobDataBean( NaModelCalcJob.CONTROL_ID, "Steuerdaten",
  // controlGML.getPath() ) };
  //    try
  //    {
  //      File baseDir = new File( "C:\\TMP\\NA_SimulationII" );
  //      baseDir.mkdirs();
  //      job.run( baseDir, beans );
  //    }
  //    catch( Exception e )
  //    {
  //      e.printStackTrace();
  //      throw e;
  //    }
  //  }

  public void testRun() throws Exception
  {
    final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
    registry.registerTypeHandler( new ObservationLinkHandler() );

    //    File baseDir = new File( "C:\\simulation\\test" );
    File baseDir = new File( "C:\\Programme\\KalypsoServer\\data\\tmp\\CalcJob-1-1099747909839" );
    File simDir = new File( baseDir, "sim" );
    File ergDir = new File( baseDir, "output" );
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

    CalcJobDataBean[] beans = new CalcJobDataBean[]
    {
        new CalcJobDataBean( NaModelCalcJob.MODELL_ID, "Modelldaten", "calc/calcCase.gml" ),
        new CalcJobDataBean( NaModelCalcJob.CONTROL_ID, "Steuerdaten", "calc/nacontrol.gml" ),
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
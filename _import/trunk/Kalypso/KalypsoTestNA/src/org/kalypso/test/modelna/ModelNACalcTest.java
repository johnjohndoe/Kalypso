/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.test.modelna;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

import javax.activation.DataHandler;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import junit.framework.TestCase;

import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.commons.diff.DiffUtils;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.convert.namodel.NaModelCalcJob;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationDataPath;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.simspec.Modeldata;
import org.kalypso.simulation.core.simspec.Modeldata.Input;
import org.kalypso.simulation.core.util.JarSimulationDataProvider;
import org.kalypso.test.util.CalcJobTestUtilis;

/**
 * Test Weisse Elster Rainfall Runoff Simulation Die CalcDataJars in die resourcen kopieren. im input.jar das
 * Rechenfall-Verzeichnis in "Rechenfall" umbenennen. (...)
 * 
 * @author doemming
 */
public class ModelNACalcTest extends TestCase
{

  // final File m_compareDir = new File( "C:\\TMP\\KalypsoCalcTestWE" );
  final File m_compareDir = new File( "C:\\KalypsoCalcTestWE" );

  /**
   * @see junit.framework.TestCase#setUp()
   */
  @Override
  protected void setUp( ) throws Exception
  {
    if( !m_compareDir.exists() )
      m_compareDir.mkdirs();
  }

  // public void XtestWE_2006_Feb( ) throws Exception
  // {
  // try
  // {
  // // calc( "we", "2006_feb", "1", false );
  // }
  // catch( Exception e )
  // {
  // e.printStackTrace();
  // throw e;
  // }
  //
  // }

  public void testWeisseElster( ) throws Exception
  {
    try
    {
      calc( "we", "test1", "1", true );
      calc( "we", "test1", "2", true );
      calc( "we", "test1", "3", true );
      calc( "we", "test1", "4", true );
      calc( "we", "test1", "5", true );
      calc( "we", "test1", "6", true );
      calc( "we", "test1", "7", true );
      calc( "we", "test1", "8", true );
      calc( "we", "test1", "9", true );
      calc( "we", "test1", "10", true );
      calc( "we", "test1", "11", true );
      calc( "we", "test1", "12", true );
      calc( "we", "test1", "13", true );
      calc( "we", "test1", "14", true );
      calc( "we", "test1", "15", true );
      calc( "we", "test1", "16", true );
      calc( "we", "test1", "17", true );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw e;
    }

  }

  /**
   * @param folder
   *          name of test folder
   * @param spec
   *          numer of modelspec.xml (e.g. "3")
   * @throws JAXBException
   * @throws IOException
   */
  public void calc( final String modellID, final String folder, String spec, final boolean doCompare ) throws JAXBException, IOException, SimulationException
  {
    final File tmpDir = CalcJobTestUtilis.getTmpDir();
    final URL resource = getClass().getResource( "testData/" + modellID + "/" + folder + "/input.jar" );
    if( spec == null || spec.length() == 0 )
      spec = "";
    else
      spec = "_" + spec;
    final URL modelSpec = getClass().getResource( "testData/" + modellID + "/modelspec" + spec + ".xml" );
    final DataHandler dataHandler = new DataHandler( resource );
    final SimulationDataPath[] beans = createBeans( modelSpec );
    final JarSimulationDataProvider jarProvider = new JarSimulationDataProvider( dataHandler, null, beans );

    final ISimulationDataProvider dataProvider = new ISimulationDataProvider()
    {

      public boolean hasID( String id )
      {
        if( NaModelConstants.IN_PARAMETER_ID.equals( id ) || NaModelConstants.IN_HYDROTOP_ID.equals( id ) )
          return true;
        if( NaModelConstants.IN_ANALYSE_MODELL_XSD_ID.equals( id ) )
          return true;
        return jarProvider.hasID( id );
      }

      public Object getInputForID( String id ) throws SimulationException
      {
        try
        {
          if( NaModelConstants.IN_HYDROTOP_ID.equals( id ) )
            return getClass().getResource( "testData/we/hydrotop.gml" );
          if( NaModelConstants.IN_PARAMETER_ID.equals( id ) )
            return getClass().getResource( "testData/we/parameter.gml" );
          if( NaModelConstants.IN_ANALYSE_MODELL_XSD_ID.equals( id ) )
            return getClass().getResource( "testData/schema/namodelTest.xsd" );
          return jarProvider.getInputForID( id );
        }
        catch( SimulationException e )
        {
          throw new SimulationException( e.getMessage(), e );
        }
      }

      public void dispose( )
      {
        // TODO Auto-generated method stub

      }
    };

    final ISimulationResultEater resultEater = CalcJobTestUtilis.createResultEater();
    final ISimulationMonitor monitor = CalcJobTestUtilis.createMonitor();

    final NaModelCalcJob job = new NaModelCalcJob();
    // final NaModelInnerCalcJob job = new NaModelInnerCalcJob()
    job.run( tmpDir, dataProvider, resultEater, monitor );

    final boolean succeeded = job.isSucceeded();
    if( succeeded )
      System.out.println( "Berechnung erzeugte Ergebnisse!" );
    else
      System.out.println( "Fehler: Berechnung erzeugte KEINE Ergebnisse!" );
    assertTrue( succeeded );

    final String identification = modellID + "_" + folder + "_" + spec;
    final File compareResults = new File( m_compareDir, identification + ".zip" );
    if( doCompare )
    {
      if( !compareResults.exists() )
      {
        System.out.println( "no comareable results found, I will archive them here:\n  " + compareResults.getAbsolutePath() );
        ZipUtilities.zip( compareResults, tmpDir );
        System.out.println( "next time you can verify changes" );
      }
      else
      {
        System.out.println( "comare results with archive: " + compareResults.getAbsolutePath() );
        final File tmpResults = File.createTempFile( identification, "zip" );
        tmpResults.deleteOnExit();
        ZipUtilities.zip( tmpResults, tmpDir );
        final String[] ignore = new String[] {
        // "inp.dat/we_nat.zft",
            // "inp.dat/we_nat.ntz",
            // "*exe",
            "*err", "*gml", "*res", "IdMap.txt", "exe.log",
            // "start/we_nat_start.txt",
            "inp.dat/we.hyd",
            // "out_we.nat/950825.qgs",
            // "inp.dat/we_nat.ger",
            "inp.dat/we_nat.geb", "zufluss/*", "klima.dat/*", "infolog.txt" };
        final ILogger logger = new ILogger()
        {
          /**
           * @see org.kalypso.contribs.java.util.logging.ILogger#log(java.util.logging.Level, int, java.lang.String)
           */
          public void log( final Level level, final int code, final String message )
          {
            System.out.println( message );
          }
        };
        assertFalse( DiffUtils.diffZips( logger, compareResults, tmpResults, ignore ) );
      }
      System.out.println( "no changes found" );
    }
  }

  private SimulationDataPath[] createBeans( final URL modelSpec ) throws JAXBException
  {
    final List<SimulationDataPath> result = new ArrayList<SimulationDataPath>();
    final JAXBContext jc = JaxbUtilities.createQuiet( org.kalypso.simulation.core.simspec.ObjectFactory.class );
    final Unmarshaller unmarshaller = jc.createUnmarshaller();
    final Modeldata modeldata = (Modeldata) unmarshaller.unmarshal( modelSpec );
    final List<Input> input = modeldata.getInput();

    for( final Input inputItem : input )
    {
      // TODO: do this only if type is uri
      String inputPath = inputItem.getPath();
      inputPath = inputPath.replaceAll( "project:/", "" );
      if( inputItem.isRelativeToCalcCase() )
        inputPath = ".prognose/Rechenfall/" + inputPath;
      result.add( new SimulationDataPath( inputItem.getId(), inputPath ) );
    }
    return result.toArray( new SimulationDataPath[result.size()] );
  }
}

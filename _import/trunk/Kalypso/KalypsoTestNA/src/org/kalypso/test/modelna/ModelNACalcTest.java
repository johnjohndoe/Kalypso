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
import java.util.Iterator;
import java.util.List;

import javax.activation.DataHandler;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import junit.framework.TestCase;

import org.kalypso.KalypsoTest;
import org.kalypso.commons.diff.DiffUtils;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.NaModelInnerCalcJob;
import org.kalypso.model.xml.Modeldata;
import org.kalypso.model.xml.ObjectFactory;
import org.kalypso.model.xml.ModeldataType.InputType;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobClientBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypso.services.calculation.service.impl.JarCalcDataProvider;
import org.kalypso.test.util.CalcJobTestUtilis;

/**
 * 
 * Test Weisse Elster Rainfall Runoff Simulation
 * 
 * Die CalcDataJars in die resourcen kopieren. im input.jar das Rechenfall-Verzeichnis in "Rechenfall" umbenennen. (...)
 * 
 * 
 * @author doemming
 */
public class ModelNACalcTest extends TestCase
{

  final File m_compareDir = new File( "C:\\TMP\\KalypsoCalcTest" );

  /**
   * @see junit.framework.TestCase#setUp()
   */
  protected void setUp() throws Exception
  {
    KalypsoTest.init();
    if( !m_compareDir.exists() )
      m_compareDir.mkdirs();
  }

  public void testWeisseElster() throws Exception
  {
    try
    {
      calc( "we", "test1", "1" );
      calc( "we", "test1", "2" ); //2.9
      calc( "we", "test1", "3" ); //1.17
      calc( "we", "test1", "4" ); //0.017
      calc( "we", "test1", "5" ); //2.8
      calc( "we", "test1", "6" );
      calc( "we", "test1", "7" );
      calc( "we", "test1", "8" );
      calc( "we", "test1", "9" );
      calc( "we", "test1", "10" );
      calc( "we", "test1", "11" );
      calc( "we", "test1", "12" );
      calc( "we", "test1", "13" );
      calc( "we", "test1", "14" );
      calc( "we", "test1", "15" );// fehler in den hydrotopen flaechen
      calc( "we", "test1", "16" );
      calc( "we", "test1", "17" );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }

  }

  /**
   * 
   * @param folder
   *          name of test folder
   * @param spec
   *          numer of modelspec.xml (e.g. "3")
   * @throws JAXBException
   * @throws IOException
   */
  public void calc( final String modellID, final String folder, String spec ) throws JAXBException, IOException
  {
    final File tmpDir = CalcJobTestUtilis.getTmpDir();
    final URL resource = getClass().getResource( "testData/" + modellID + "/" + folder + "/input.jar" );
    if( spec == null || spec.length() == 0 )
      spec = "";
    else
      spec = "_" + spec;
    final URL modelSpec = getClass().getResource( "testData/" + modellID + "/modelspec" + spec + ".xml" );
    final DataHandler dataHandler = new DataHandler( resource );
    final CalcJobClientBean[] beans = createBeans( modelSpec );
    final ICalcDataProvider dataProvider = new JarCalcDataProvider( dataHandler, beans )
    {
      public boolean hasID( String id )
      {
        if( NaModelConstants.IN_HYDROTOP_ID.equals( id ) )
          return true;
        return super.hasID( id );
      }

      /**
       * @see org.kalypso.services.calculation.service.impl.JarCalcDataProvider#getURLForID(java.lang.String)
       */
      public URL getURLForID( String id ) throws CalcJobServiceException
      {
        if( NaModelConstants.IN_HYDROTOP_ID.equals( id ) )
          return getClass().getResource( "testData/we/hydrotop.gml" );
        return super.getURLForID( id );
      }
    };

    final ICalcResultEater resultEater = CalcJobTestUtilis.createResultEater();
    final ICalcMonitor monitor = CalcJobTestUtilis.createMonitor();
    final NaModelInnerCalcJob job = new NaModelInnerCalcJob();
    job.run( tmpDir, dataProvider, resultEater, monitor );

    assertTrue( job.isSucceeded() );
    final String identification = modellID + "_" + folder + "_" + spec;
    final File compareResults = new File( m_compareDir, identification + ".zip" );
    if( !compareResults.exists() )
    {
      System.out.println( "no comareable results found, I will archive them here:\n  "
          + compareResults.getAbsolutePath() );
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
      //          "inp.dat/we_nat.ntz",
      //          "infolog.txt",
      //          "*debug.txt",
      //          "*exe",
      //          "*err",
      //          "*gml",
      //          "*res",
      //          "IdMap.txt",
      //          "exe.log",
      //          "start/we_nat_start.txt",
      //          "inp.dat/we.hyd",
      //          "out_we.nat/950825.qgs",
      //          "inp.dat/we_nat.ger",
      //          "inp.dat/we_nat.geb",
      //          "zufluss/*",
      };
      ILogger logger = new ILogger()
      {
        /**
         * @see org.kalypso.contribs.java.util.logging.ILogger#log(java.lang.String)
         */
        public void log( String message )
        {
          System.out.println( message );
        }
      };
      assertFalse( DiffUtils.diffZips( logger, compareResults, tmpResults, ignore ) );
      System.out.println( "no changes found" );
    }
  }

  private CalcJobClientBean[] createBeans( URL modelSpec ) throws JAXBException
  {
    final List result = new ArrayList();
    final ObjectFactory objectFactory = new ObjectFactory();
    final Unmarshaller unmarshaller = objectFactory.createUnmarshaller();
    final Modeldata modeldata = (Modeldata)unmarshaller.unmarshal( modelSpec );
    final List input = modeldata.getInput();

    for( Iterator iter = input.iterator(); iter.hasNext(); )
    {

      final InputType inputItem = (InputType)iter.next();

      String inputPath = inputItem.getPath();
      inputPath = inputPath.replaceAll( "project:/", "" );
      if( inputItem.isRelativeToCalcCase() )
        inputPath = ".prognose/Rechenfall/" + inputPath;
      result.add( new CalcJobClientBean( inputItem.getId(), inputPath ) );
    }
    return (CalcJobClientBean[])result.toArray( new CalcJobClientBean[result.size()] );
  }
}

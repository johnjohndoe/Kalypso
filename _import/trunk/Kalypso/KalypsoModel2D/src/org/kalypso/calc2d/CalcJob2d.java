/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of ekalypso:
 Internet based elearning for complex simulation applications
 ("Internet basiertes E-Learning an komplexen Simulationsprogrammen [de]")

 The implementation is realised by: 
 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 The project is sponsored and supported by:  
 local authority of education and research, 
 E-Learning Consortium Hamburg (ELCH) and
 Multimedia Kontor Hamburg.
 
 As this implementation depends on third party open source 
 java code it is consequently also licenced as open source
 in the hope that it will be useful specially (but not exclusively)
 to other e-learning projects that may extend or use this project.
 
 Copyright (C) 2004, 2005 by:
 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

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
 katharina.lupp@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
/**
 * Created on 07.03.2005
 */

package org.kalypso.calc2d;

import java.io.File;
import java.io.FileFilter;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.net.URL;

import org.apache.commons.io.CopyUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.kalypso.convert.model2d.ConvertAsci2GML;
import org.kalypso.convert.model2d.ConvertBC2Ascii;
import org.kalypso.convert.model2d.ConvertGML2Asci;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public class CalcJob2d implements ICalcJob
{

  private final String EXE_FILE = "StartSimulation.bat";

  public static final String MODELL_ID = "Modell";

  public static final String CONTROL_ID = "Control";

  public static final String RESULTS_ID = "ERGEBNISSE";

  private boolean succeeded = false;

  private final String CONF_FILE = "SimConfig.txt";

  private final String SIM_EXE_FILE = "Kalypso_2D_vers1_2_3_large.exe";

  private final String DGM_FILE = "work.dgn";

  private final static String ID_RESULTS = "ERGEBNISSE";

  private static final String ID_LOG = "LOG";

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
  {
    return getClass().getResource( "2d_spec.xml" );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider,
   *      org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( File tmpdir, ICalcDataProvider inputProvider, ICalcResultEater resultEater,
      ICalcMonitor monitor ) throws CalcJobServiceException
  {
    final File outputDir;

    final URL schemaModellURL = getClass().getResource( "schema/2dgml.xsd" );
    final URL schemaControlURL = getClass().getResource( "schema/bc_gml2.xsd" );
    final File exeDir = new File( tmpdir, "simulation" );
    exeDir.mkdirs();

    //    File result = new File( exeDir, "test.txt" );
    //    FileWriter writer = null;
    //    try
    //    {
    //      writer = new FileWriter( result );
    //      writer.write( "pseudo ergebnis" );
    //    }
    //    catch( IOException e )
    //    {
    //      e.printStackTrace();
    //    }
    //    finally
    //    {
    //      IOUtils.closeQuietly( writer );
    //    }
    //
    //    resultEater.addResult( ID_RESULTS, exeDir );
    //    resultEater.addResult( ID_LOG, result );

    try
    {
      monitor.setMessage( "creating file system for simulation..." );
      if( monitor.isCanceled() )
        return;

      monitor.setMessage( "generating ascii files for 2D simulation..." );

      ConvertGML2Asci gml2asci = new ConvertGML2Asci( exeDir );
      gml2asci.convertGML2Asci( inputProvider.getURLForID( MODELL_ID ), schemaModellURL );
      monitor.setMessage( "generating mesh ascii file" );

      ConvertBC2Ascii bc2asci = new ConvertBC2Ascii( exeDir );
      bc2asci.convertBC2Ascii( inputProvider.getURLForID( CONTROL_ID ), schemaControlURL,
          inputProvider.getURLForID( MODELL_ID ), schemaModellURL );
      monitor.setMessage( "generating boundary conditions ascii file" );

      copySim( exeDir );

      monitor.setMessage( "starting 2D simulation..." );

      if( monitor.isCanceled() )
        return;
      monitor.setProgress( 17 );
      startCalculation( monitor, exeDir );
      checkSucceeded( monitor, exeDir );

      if( isSucceeded() )
      {
        monitor.setMessage( "loading results..." );
        outputDir = new File( tmpdir, "Ergebnisse" );
        outputDir.mkdirs();

        FileFilter suffixFileFilter = FileFilterUtils.suffixFileFilter( ".2d" );
        File[] files = exeDir.listFiles( suffixFileFilter );

        monitor.setProgress( 80 );
        for( int i = 0; i < files.length; i++ )
        {
          System.out.println( "name of file_" + i + ":: " + files[i].getName() );
          if( !files[i].getName().equalsIgnoreCase( "erg.2d" )
              && !files[i].getName().equalsIgnoreCase( "fehler.2d" )
              && !files[i].getName().equalsIgnoreCase( "out.2d" )
              && !files[i].getName().equalsIgnoreCase( "marsh.2d" ) )
          {
            addResult( resultEater, files[i], outputDir, exeDir );
          }
          monitor.setProgress( 99 );
        }
        resultEater.addResult( ID_RESULTS, outputDir );

        System.out.println( "Finished 2D Simulation successfully." );

        System.out.println( "This is the end ;-)" );
      }
      else
        System.out.println( "Finished 2D Simulation not successfully." );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "simulation couldn't be finished", e );
    }
  }

  private void addResult( ICalcResultEater resultEater, File file, File outputDir, File exeDir )
  {
    try
    {
      final URL schemaModellXMLURL = getClass().getResource( "schema/2d.xsd" );
      String fileName = file.getName();
      if( !fileName.equalsIgnoreCase( "erg.2d" ) && !fileName.equalsIgnoreCase( "marsh.2d" )
          && !fileName.equalsIgnoreCase( "out.2d" ) && !fileName.equalsIgnoreCase( "fehler" ) )
      {

        int pos = fileName.indexOf( "." );
        String name = fileName.substring( 0, pos );
        String path = file.getPath();
        int i = path.lastIndexOf( "\\" );
        path = path.substring( 0, i + 1 );
        String tmpXMLFile = path + "tmp.xml";
        String gmlFileName = outputDir + "\\" + name + ".gml";

        ConvertAsci2GML gmlFile = new ConvertAsci2GML();
        File gml = gmlFile.convertAsci2GML( file.toString(), tmpXMLFile,
            "http://elbe.wb.tu-harburg.de", schemaModellXMLURL.toString(), gmlFileName );

        StringBuffer sb = new StringBuffer();   
        sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>");
        sb.append("<gismapview xmlns=\"gismapview.template.kalypso.org\">");
        sb.append("<extent bottom=\"5978152.499976501\" left=\"3545304.474577534\" right=\"3548446.416650312\" top=\"5981196.0\"/>");
        sb.append("<layers active=\"ID_2\">");
        sb.append(" <layer name=\"Knoten\" visible=\"true\" featurePath=\"featurePointCollectionMember/featurePointMember\" id=\"ID_1\" linktype=\"gml\" ns1:actuate=\"onRequest\" " +
            "ns1:href=\""+name+".gml\" ns1:type=\"simple\" xmlns:ns1=\"http://www.w3.org/1999/xlink\">");
        sb.append("<ns2:style linktype=\"sld\" style=\"FEM\" ns1:actuate=\"onRequest\" ns1:href=\"project:/.styles/sldPfeile.sld\" ns1:type=\"simple\" xmlns:ns2=\"types.template.kalypso.org\"/>");
        sb.append("</layer>");
        sb.append("<layer name=\"FEM\" visible=\"true\" featurePath=\"femCollectionMember/meshMember\" id=\"ID_2\" linktype=\"gml\" ns3:actuate=\"onRequest\" " +
            "ns3:href=\""+name+".gml\" ns3:type=\"simple\" xmlns:ns3=\"http://www.w3.org/1999/xlink\">");
        sb.append("<ns4:style linktype=\"sld\" style=\"FEM\" ns3:actuate=\"onRequest\" ns3:href=\"project:/.styles/sldResults.sld\" ns3:type=\"simple\" xmlns:ns4=\"types.template.kalypso.org\"/>");
        sb.append(" </layer>");
        sb.append("</layers>");
        sb.append("</gismapview>");
        
        OutputStreamWriter writer = new OutputStreamWriter(
            new FileOutputStream(new File(outputDir, name+".gmt")), "UTF-8");
        writer.write(sb.toString());
        writer.close();
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * copies files necessary for 2d simulation to start
   */
  private void copySim( File basedir ) throws IOException
  {

    final File simExeFile = new File( basedir, SIM_EXE_FILE );
    final File configFile = new File( basedir, CONF_FILE );
    final File dgmFile = new File( basedir, DGM_FILE );
    final File exeFile = new File( basedir, EXE_FILE );

    final InputStream simInputStream = getClass().getResourceAsStream( SIM_EXE_FILE );
    final InputStream exeInputStream = getClass().getResourceAsStream( EXE_FILE );
    final InputStream confInputStream = getClass().getResourceAsStream( CONF_FILE );
    final InputStream dgmInputStream = getClass().getResourceAsStream( DGM_FILE );

    FileUtilities.makeFileFromStream( false, simExeFile, simInputStream );
    FileUtilities.makeFileFromStream( false, exeFile, exeInputStream );
    FileUtilities.makeFileFromStream( false, configFile, confInputStream );
    FileUtilities.makeFileFromStream( false, dgmFile, dgmInputStream );
  }

  /**
   * @return boolean succeeded
   */
  public boolean isSucceeded()
  {
    return this.succeeded;
  }

  /**
   * starts 2D simulation
   * 
   * @param monitor
   */
  private void startCalculation( ICalcMonitor monitor, final File basedir )
      throws CalcJobServiceException
  {
    InputStreamReader inStream = null;
    InputStreamReader errStream = null;
    PrintWriter outWriter = null;
    PrintWriter errWriter = null;

    try
    {
      final File exeFile = new File( basedir, EXE_FILE );
      final File exeDir = exeFile.getParentFile();
      final String commandString = exeFile.getAbsolutePath();
      System.out.println( "commandString: " + commandString );

      final Process process = Runtime.getRuntime().exec( commandString, null, exeDir );

      outWriter = new PrintWriter( new FileWriter( new File( basedir, "exe.log" ) ) );
      errWriter = new PrintWriter( new FileWriter( new File( basedir, "exe.err" ) ) );

      inStream = new InputStreamReader( process.getInputStream() );
      errStream = new InputStreamReader( process.getErrorStream() );
      while( true )
      {
        CopyUtils.copy( inStream, outWriter );
        CopyUtils.copy( errStream, errWriter );
        try
        {
          process.exitValue();
          return;
        }
        catch( IllegalThreadStateException e )
        {
          e.printStackTrace();
        }

        if( monitor.isCanceled() )
        {
          process.destroy();
          return;
        }
        Thread.sleep( 100 );
      }
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "error occurred...", e );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "error occurred...", e );
    }
    finally
    {
      try
      {
        if( outWriter != null )
          outWriter.close();

        if( errWriter != null )
          errWriter.close();

        if( inStream != null )
          inStream.close();

        if( errStream != null )
          errStream.close();
      }
      catch( final IOException e1 )
      {
        e1.printStackTrace();
      }
    }
  }

  /**
   * checks if calculation of simulations is succeeded
   */
  public void checkSucceeded( ICalcMonitor monitor, final File inputDir )
  {
    Reader logFileReader = null;
    LineNumberReader reader = null;
    try
    {
      final File logFile = new File( inputDir, "out.2d" );
      logFileReader = new FileReader( logFile );
      reader = new LineNumberReader( logFileReader );
      String line;
      while( ( line = reader.readLine() ) != null )
      {
        if( line.indexOf( " KALYPSO-2D: instationaere Berechnung ordnungsgemaess gelaufen" ) >= 0 )
          succeeded = true;
      }

      if( isSucceeded() )
      {
        monitor.setMessage( "loading results" );
      }

    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( reader );
      IOUtils.closeQuietly( logFileReader );
    }
  }

}
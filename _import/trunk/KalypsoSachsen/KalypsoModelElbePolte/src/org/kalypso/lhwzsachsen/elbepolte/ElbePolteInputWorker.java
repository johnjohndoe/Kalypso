/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
 *  g.belger@bjoernsen.de
 *  m.schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.lhwzsachsen.elbepolte;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.URL;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileCopyVisitor;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.lhwzsachsen.elbepolte.visitors.FeatureVisitorZml2Hwvs;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import com.braju.format.Format;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author thuel2
 */
public class ElbePolteInputWorker
{

  /**
   *  
   */

  public ElbePolteInputWorker()
  {
  // will not be instantiated
  }

  /**
   * @param tmpdir
   * @param inputProvider
   * @param props
   * @param nativeindir
   * @param pw
   * @return
   * @throws Exception
   */
  public static File createNativeInput( File tmpdir, ICalcDataProvider inputProvider, PrintWriter logwriter,
      Properties props, File nativeindir ) throws Exception
  {
    try
    {
      final File exeDir = new File( tmpdir, "exe" );
      exeDir.mkdirs();

      final URL controlGmlURL = inputProvider.getURLForID( "CONTROL_GML" );

      logwriter.println( "Lese Steuerparameter: " + controlGmlURL.toString() );

      final Map map = parseControlFile( controlGmlURL );
      props.putAll( map );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( inputProvider.getURLForID( "GML" ) );

      props.put( ElbePolteConst.DATA_GML, workspace );

      logwriter.println( "Schreibe Parameterdatei" );
      writeParFile( workspace, exeDir, logwriter );

      logwriter.println( "Erzeuge Zeitreihen" );
      // TODO hat das was mit den Umhüllenden zu tun???
      //      applyAccuracyPrediction( workspace, tsmap );
      writeTimeseries( props, exeDir, inputProvider.getURLForID( "GML" ) );
      
      // TODO kopiere noch Inhalt von exeDir nach nativeIn, damit der User die Rechenvarianten nachvollziehen kann
      
      if( exeDir.exists() && nativeindir.exists() )
      {
        final FileCopyVisitor copyVisitor = new FileCopyVisitor( exeDir, nativeindir, true );
        FileUtilities.accept( exeDir, copyVisitor, true );
      }
     
      logwriter.println( "Kopiere Rechenkern" );
      copyAndUnzipRechenkern( exeDir );

      return exeDir;
    }
    catch( final CalcJobServiceException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Erzeugen der Inputdateien", e );
    }
  }

  /**
   * @param props
   * @param nativedir
   * @throws Exception
   */
  private static void writeTimeseries( Properties props, File nativedir, URL context ) throws Exception
  {
    // workspace holen
    final GMLWorkspace wks = (GMLWorkspace)props.get( ElbePolteConst.DATA_GML );
    // für die einzelnen Pegeltypen Zeitreihen schreiben
    final File modelldir = new File( nativedir, "Modell" );
    modelldir.mkdirs();
    final File datendir = new File( nativedir, "Daten" );
    datendir.mkdirs();

    // Zeitreihen an Startpegeln
    FeatureVisitorZml2Hwvs.writeTimeseries( wks, ElbePolteConst.GML_START_PEGEL_COLL, "ganglinie_gesamt", "nr",
        "Modell", modelldir, context );
    // Zeitreihen der Zwischengebietszuflüsse
    FeatureVisitorZml2Hwvs.writeTimeseries( wks, ElbePolteConst.GML_ZWG_ZUFLUSS_COLL, "ganglinie_gesamt", "nr",
        "Modell", modelldir, context );
    // Zeitreihen an Elbepegeln
    FeatureVisitorZml2Hwvs.writeTimeseries( wks, ElbePolteConst.GML_ELBE_PEGEL_COLL, "ganglinie_messwerte", "nr",
        "Daten", datendir, context );

  }

  /**
   * @param nativedir
   * @throws CalcJobServiceException
   */
  private static void copyAndUnzipRechenkern( File nativedir ) throws CalcJobServiceException
  {
    final URL urlRk;
    InputStream zipStream = null;

    // rechenkern.zip aus den resourcen holen
    urlRk = ElbePolteInputWorker.class.getResource( "resources/" + ElbePolteConst.RECHENKERN_ZIP );

    try
    {
      // rechenkern.zip in nativedir entpacken
      zipStream = urlRk.openStream();
      ZipUtilities.unzip( zipStream, nativedir );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Entpacken des Rechenkerns", e );
    }
    finally
    {
      IOUtils.closeQuietly( zipStream );
    }
  }

  /**
   * @param workspace
   * @param nativedir
   * @param logwriter
   * @throws Exception
   */
  private static void writeParFile( GMLWorkspace workspace, File nativedir, PrintWriter logwriter ) throws Exception
  {
    PrintWriter pWrtr = null;
    final File outfile = new File( nativedir, "ObereElbe.PAR" );
    final FileOutputStream stream;
    try
    {
      stream = new FileOutputStream( outfile );

      pWrtr = new PrintWriter( new BufferedWriter( new OutputStreamWriter( stream, ElbePolteConst.ELBEPOLTE_CODEPAGE ) ) );

      // Allgemeine Modellparams
      final Feature modell = workspace.getRootFeature();
      writeGeneralModelParams( modell, pWrtr, logwriter );
      // Streckenparams
      final FeatureList streckeList = (FeatureList)workspace.getFeatureFromPath( ElbePolteConst.GML_STRECKE_COLL );
      writeStreckeParams( streckeList, pWrtr, logwriter );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      logwriter.println( e.getLocalizedMessage() );
      throw new Exception( "Fehler beim Schreiben der Parameterdatei.", e );
    }
    finally
    {
      IOUtils.closeQuietly( pWrtr );
    }
  }

  /**
   * @param streckeList
   * @param wrtr
   * @param logwriter
   * @throws IOException
   */
  private static void writeStreckeParams( FeatureList streckeList, PrintWriter wrtr, PrintWriter logwriter )
      throws IOException
  {
    logwriter.println( "- Streckenparameter" );

    for( final Iterator itStrecke = streckeList.iterator(); itStrecke.hasNext(); )
    {
      final Feature featStrecke = (Feature)itStrecke.next();
      // eigentlich nach Strecke zu sortieren
      // allgmeine Info zur Strecke (Nr., DB, LT, Zwg-Zuschlag, Messwerte ersetzen?, nrFEKO) + Name
      final String nr = (String)featStrecke.getProperty( "nr" );
      final double db = ( (Double)featStrecke.getProperty( "db" ) ).doubleValue();
      final int lt = ( (Integer)featStrecke.getProperty( "lt" ) ).intValue();
      final double zwgZuschlag = ( (Double)featStrecke.getProperty( "zwg_zuschlag" ) ).doubleValue();
      final String replaceVals = ( (Boolean)featStrecke.getProperty( "replaceValues" ) ).booleanValue() ? "1" : "0";
      final int nrFeko = ( (Integer)featStrecke.getProperty( "nr_feko" ) ).intValue();

      final String sep = ElbePolteConst.PAR_FILE_SEP;
      wrtr.println( nr + sep + db + sep + lt + sep + zwgZuschlag + sep + replaceVals + sep + nrFeko );
      wrtr.println( (String)featStrecke.getProperty( "name" ) );

      // ParamSets
      final FeatureList paramSetList = (FeatureList)featStrecke.getProperty( "paramSetMember" );
      writeStreckeParamSets( paramSetList, wrtr, logwriter );
    }

  }

  /**
   * @param paramSetList
   * @param wrtr
   * @param logwriter
   * @throws IOException
   */
  private static void writeStreckeParamSets( FeatureList paramSetList, PrintWriter wrtr, PrintWriter logwriter )
      throws IOException
  {

    for( final Iterator itParamSet = paramSetList.iterator(); itParamSet.hasNext(); )
    {
      final Feature featParamSet = (Feature)itParamSet.next();
      // eigentlich nach hw_type zu sortieren...
      // QL1, QL2, km, ce, is
      final double ql1 = ( (Double)featParamSet.getProperty( "QL1" ) ).doubleValue();
      final double ql2 = ( (Double)featParamSet.getProperty( "QL2" ) ).doubleValue();
      final double km = ( (Double)featParamSet.getProperty( "km" ) ).doubleValue();
      final double ce = ( (Double)featParamSet.getProperty( "ce" ) ).doubleValue();
      final double is = ( (Double)featParamSet.getProperty( "is" ) ).doubleValue();

      final String sep = ElbePolteConst.PAR_FILE_SEP;
      wrtr.println( ql1 + sep + ql2 + sep + km + sep + ce + sep + is );

      // StufenParamSets
      final FeatureList stufenParamSetList = (FeatureList)featParamSet.getProperty( "stufenParamSetMember" );
      writeStreckeStufenParamSets( stufenParamSetList, wrtr, logwriter );

    }
  }

  /**
   * @param stufenParamSetList
   * @param wrtr
   * @param logwriter
   * @throws IOException
   */
  private static void writeStreckeStufenParamSets( FeatureList stufenParamSetList, PrintWriter wrtr,
      PrintWriter logwriter ) throws IOException
  {

    final String sep = ElbePolteConst.PAR_FILE_SEP;
    final int cntSPS = stufenParamSetList.size();
    for( final Iterator itStufenParamSet = stufenParamSetList.iterator(); itStufenParamSet.hasNext(); )
    {
      final Feature featStufenParamSet = (Feature)itStufenParamSet.next();
      // müssen die auch sortiert werden?
      // tl, Anzahl b, a1, a2, b1, ..., bn, f
      final double tl = ( (Double)featStufenParamSet.getProperty( "tl" ) ).doubleValue();
      Format.fprintf( wrtr, ElbePolteConst.PAR_NUMBER_FORMAT_LANG, new Object[]
      { new Double( tl ) } );
      wrtr.print( sep );

      final double a1 = ( (Double)featStufenParamSet.getProperty( "a1" ) ).doubleValue();
      Format.fprintf( wrtr, ElbePolteConst.PAR_NUMBER_FORMAT_LANG, new Object[]
      { new Double( a1 ) } );
      wrtr.print( sep );

      final double a2 = ( (Double)featStufenParamSet.getProperty( "a2" ) ).doubleValue();
      Format.fprintf( wrtr, ElbePolteConst.PAR_NUMBER_FORMAT_LANG, new Object[]
      { new Double( a2 ) } );

      // bMembers
      final FeatureList bList = (FeatureList)featStufenParamSet.getProperty( "bMember" );
      final int cntB = bList.size();
      wrtr.print( sep + cntB + sep );

      for( final Iterator itB = bList.iterator(); itB.hasNext(); )
      {
        final Feature featB = (Feature)itB.next();
        Format.fprintf( wrtr, ElbePolteConst.PAR_NUMBER_FORMAT_LANG, new Object[]
        { (Double)featB.getProperty( "b" ) } );
        wrtr.print( sep );
      }

      final Object fProp = featStufenParamSet.getProperty( "f" );

      if( fProp != null )
      {
        Format.fprintf( wrtr, ElbePolteConst.PAR_NUMBER_FORMAT_LANG, new Object[]
        { fProp } );
      }
      wrtr.println();
    }

    for( int ii = cntSPS; ii < 3; ii++ )
    {
      wrtr.println( "-1" );
    }
  }

  /**
   * @param modell
   * @param wrtr
   * @param logwriter
   */
  private static void writeGeneralModelParams( Feature modell, PrintWriter wrtr, PrintWriter logwriter )
  {
    logwriter.println( "- allg. Modellparamter" );
    final String hwTyp = ( modell.getProperty( "hw_type" ) ).toString();
    final String nachfRIDO = ( (Boolean)modell.getProperty( "rido" ) ).booleanValue() ? "1" : "0";
    wrtr.println( hwTyp + ElbePolteConst.PAR_FILE_SEP + nachfRIDO );
  }

  public static Map parseControlFile( final URL gmlURL ) throws CalcJobServiceException
  {
    try
    {
      final Feature controlFeature = GmlSerializer.createGMLWorkspace( gmlURL ).getRootFeature();

      final Date startForecastTime = (Date)controlFeature.getProperty( "startforecast" );

      final Map dataMap = new HashMap();
      dataMap.put( ElbePolteConst.DATA_STARTFORECAST_DATE, startForecastTime );

      return dataMap;
    }
    catch( final Exception e )
    {
      throw new CalcJobServiceException( "Fehler beim Einlesen der Berechnungsparameter", e );
    }
  }
}

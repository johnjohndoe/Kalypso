package org.kalypso.lhwsachsenanhalt.tubig;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.io.IOUtils;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.extension.TypeRegistrySingleton;

import com.braju.format.Format;

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
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/

/**
 * Lesen und Schreiben der Speicher-Parameter <br>
 * Verwendung für die Tubig-Modelle 
 * 
 * @author Thül
 */

public class TubigExportParameter
{

  /**
   * writeSpeicherPars <br>
   * schreibt Speicherparameter-Dateien (*.sra, *.qmi, *.qma, *.twa, *.lea) <br>
   * für alle "echten" Speicher aus dem GML-Workspace in das angegebene
   * Verzeichnis
   * 
   * @param workspace
   * @param dir
   */
  public static void writeSpeicherPars( final GMLWorkspace workspace, final File dir )
  {
    //String sName;
    String sKurzName;
    Iterator itSpeicher;
    Feature featSpeicher;

    final FeatureList speicherlist = (FeatureList)workspace
        .getFeatureFromPath( TubigConst.GML_SPEICHER_COLL );
    for( itSpeicher = speicherlist.iterator(); itSpeicher.hasNext(); )
    {
      featSpeicher = (Feature)itSpeicher.next();

      //sName = (String)featSpeicher.getProperty( "Name" );
      sKurzName = (String)featSpeicher.getProperty( TubigConst.GML_KURZ_NAME );

      writeSpeicherStauraum( featSpeicher, dir, sKurzName );
      writeSpeicherMindestabgabe( featSpeicher, dir, sKurzName );
      writeSpeicherMaximalabgabe( featSpeicher, dir, sKurzName );
      writeSpeicherTrinkwasser( featSpeicher, dir, sKurzName );
      writeSpeicherEntlastungsanlagen( featSpeicher, dir, sKurzName );

    }
  }

  private static void writeSpeicherStauraum( final Feature speicher, final File dir,
      final String kurzname )
  {
    final Feature featStauraum;
    final Double dTotraum;
    final Double dReserveraum;
    final Double dStauraum;
    final Feature featBetriebsraumJahr;
    final FeatureList featLstBetriebsraum;
    final File outfile;
    final FileOutputStream stream;

    PrintWriter pWrtr;
    Feature featMonat;
    Iterator iter;

    pWrtr = null;
    outfile = new File( dir, kurzname + ".sra" );
    try
    {
      stream = new FileOutputStream( outfile );

      pWrtr = new PrintWriter( new BufferedWriter( new OutputStreamWriter( stream,
          TubigConst.TUBIG_CODEPAGE ) ) );

      featStauraum = (Feature)speicher.getProperty( "StauraumParameter" );
      dTotraum = (Double)featStauraum.getProperty( "Totraum" );
      dReserveraum = (Double)featStauraum.getProperty( "Reserveraum" );
      dStauraum = (Double)featStauraum.getProperty( "Stauraum" );

      featBetriebsraumJahr = (Feature)featStauraum.getProperty( "Betriebsraum" );
      featLstBetriebsraum = (FeatureList)featBetriebsraumJahr.getProperty( "MonatMember" );

      // Kommentar, Totraum [Mio. m³], Reserveraum [Mio. m³],
      // Betriebsraum Jan-Dez [Mio. m³], ges. Stauraum [Mio. m³]
      pWrtr.println( "REM " + outfile.getName() );
      Format.fprintf( pWrtr, TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
      { dTotraum } );
      pWrtr.println();
      Format.fprintf( pWrtr, TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
      { dReserveraum } );
      pWrtr.println();
      for( iter = featLstBetriebsraum.iterator(); iter.hasNext(); )
      {
        featMonat = (Feature)iter.next();
        Format.fprintf( pWrtr, TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
        { featMonat.getProperty( "Wert" ) } );
        pWrtr.println();
      }
      Format.fprintf( pWrtr, TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
      { dStauraum } );
      pWrtr.println();
      pWrtr.close();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( pWrtr );
    }
  }

  private static void writeSpeicherKommentarUndJahr( final Feature speicher, final File dir,
      final String dateiName, final String sFeatProp1, final String sFeatProp2,
      final String sFeatLstProp1, final boolean bAddComment )
  {
    final Feature featMindestabgabe;
    final Feature featMindestabgabeJahr;
    final FeatureList featLstMindestabgabe;
    final File outfile;
    final FileOutputStream stream;

    Feature featMonat;
    Iterator iter;
    PrintWriter pWrtr;

    pWrtr = null;
    outfile = new File( dir, dateiName );
    try
    {
      stream = new FileOutputStream( outfile );

      pWrtr = new PrintWriter( new BufferedWriter( new OutputStreamWriter( stream,
          TubigConst.TUBIG_CODEPAGE ) ) );

      featMindestabgabe = (Feature)speicher.getProperty( sFeatProp1 );

      featMindestabgabeJahr = (Feature)featMindestabgabe.getProperty( sFeatProp2 );
      featLstMindestabgabe = (FeatureList)featMindestabgabeJahr.getProperty( sFeatLstProp1 );

      // Kommentar, ggf. Zusatzzeile (f. Trinkwasser: Zahl!, die angibt in
      // welcher Einheit die Werte in der WinPro-Oberfläche angezeigt werden,
      // hier nur Dummy-Funktion),
      // Monatswerte (Jan-Dez)
      pWrtr.println( "REM " + outfile.getName() );
      if( bAddComment )
        pWrtr.println( "1 " );
      for( iter = featLstMindestabgabe.iterator(); iter.hasNext(); )
      {
        featMonat = (Feature)iter.next();
        Format.fprintf( pWrtr, TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
        { featMonat.getProperty( "Wert" ) } );
        pWrtr.println();
      }
      pWrtr.close();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( pWrtr );
    }
  }

  private static void writeSpeicherMindestabgabe( final Feature speicher, final File dir,
      final String kurzname )
  {
    // [m³/s]
    writeSpeicherKommentarUndJahr( speicher, dir, kurzname + ".qmi", "MindestabgabeParameter",
        "Mindestabgabe", "MonatMember", false );
  }

  private static void writeSpeicherMaximalabgabe( final Feature speicher, final File dir,
      final String kurzname )
  {
    // [m³/s]
    writeSpeicherKommentarUndJahr( speicher, dir, kurzname + ".qma", "MaximalabgabeParameter",
        "Maximalabgabe", "MonatMember", false );
  }

  private static void writeSpeicherTrinkwasser( final Feature speicher, final File dir,
      final String kurzname )
  {
    // [m³/s]
    writeSpeicherKommentarUndJahr( speicher, dir, kurzname + ".twa", "TrinkwasserParameter",
        "Trinkwasser", "MonatMember", true );
  }

  private static void writeSpeicherEntlastungsanlagen( final Feature speicher, final File dir,
      final String kurzname )
  {
    final Feature featEntlastungsanlagen;
    final FeatureList featLstEntlastungen;
    final File outfile;
    final FileOutputStream stream;
    final Map leaMap = new HashMap();
    final TreeMap leaTreeMap;

    Iterator iter;
    Feature featEntlastung;
    String sBemerkung;
    String sLeaZeile;
    int iNum;
    Double dUeberlauf;
    Double dHoehe;
    PrintWriter pWrtr;
    Map.Entry mapEntry;

    pWrtr = null;
    outfile = new File( dir, kurzname + ".lea" );

    try
    {
      stream = new FileOutputStream( outfile );

      pWrtr = new PrintWriter( new BufferedWriter( new OutputStreamWriter( stream,
          TubigConst.TUBIG_CODEPAGE ) ) );

      featEntlastungsanlagen = (Feature)speicher.getProperty( "EACollectionAssociation" );
      featLstEntlastungen = (FeatureList)featEntlastungsanlagen.getProperty( "EAMember" );

      // Kommentar, Anzahl der Stützstellen, Stützstelle, bei der Überlauf
      // beginnt, tabellarische Info zu den Entlastungsanlagen
      pWrtr.println( "REM " + outfile.getName() );
      pWrtr.println( featLstEntlastungen.size() );

      // Features nach Höhe sortiert in Datei eintragen
      iNum = -1;
      for( iter = featLstEntlastungen.iterator(); iter.hasNext(); )
      {
        featEntlastung = (Feature)iter.next();

        sLeaZeile = Format.sprintf( TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
        { featEntlastung.getProperty( "Höhe" ) } );
        dHoehe = (Double)featEntlastung.getProperty( "Höhe" );
        sLeaZeile = sLeaZeile + TubigConst.TUBIG_SEP + Format.sprintf( TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
        { featEntlastung.getProperty( "Inhalt" ) } );
        sLeaZeile = sLeaZeile + TubigConst.TUBIG_SEP + Format.sprintf( TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
        { featEntlastung.getProperty( "Grundablass" ) } );
        sLeaZeile = sLeaZeile + TubigConst.TUBIG_SEP + Format.sprintf( TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
        { featEntlastung.getProperty( "Überlauf" ) } );
        dUeberlauf = (Double)featEntlastung.getProperty( "Überlauf" );
        if( dUeberlauf.doubleValue() <= 0.0 )
        {
          iNum = iNum + 1;
        }
        sBemerkung = (String)featEntlastung.getProperty( "Bemerkung" );
        if( sBemerkung != null )
          sLeaZeile = sLeaZeile + TubigConst.TUBIG_SEP + Format.sprintf( TubigConst.TUBIG_STRING_FORMAT, new Object[]
          { TubigConst.TUBIG_SEP + sBemerkung } );
        leaMap.put( dHoehe, sLeaZeile );
      }

      pWrtr.println( iNum );

      leaTreeMap = new TreeMap( leaMap );
      iter = leaTreeMap.entrySet().iterator();
      while( iter.hasNext() )
      {
        mapEntry = (Map.Entry)iter.next();
        pWrtr.println( mapEntry.getValue() );
      }

      pWrtr.close();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( pWrtr );
    }
  }

  /**
   * writePegelPars <br>
   * schreibt Parameter-Dateien <br>
   * für alle Pegel aus dem Workspace in das angegebene Verzeichnis derzeit
   * werden nur WLM-Parameter-Dateien (wlm_ <pegel>.par) geschrieben <br>
   * 
   * @param workspace
   * @param dir
   */
  public static void writePegelPars( final GMLWorkspace workspace, final File dir )
  {
    //String sName;
    String sKurzName;
    Iterator itPegel;
    Feature featPegel;
    FeatureList featLstPegel;

    // Parameter-Dateien für Wasserlaufmodelle
    featLstPegel = (FeatureList)workspace.getFeatureFromPath( TubigConst.GML_WLM_COLL );
    for( itPegel = featLstPegel.iterator(); itPegel.hasNext(); )
    {
      featPegel = (Feature)itPegel.next();

      //sName = (String)featPegel.getProperty( "Name" );
      sKurzName = (String)featPegel.getProperty( TubigConst.GML_KURZ_NAME );

      writePegelWlmPars( featPegel, dir, sKurzName );
    }
  }

  private static void writePegelWlmPars( final Feature featPegel, final File dir,
      final String kurzname )
  {
    final File outfile;
    final FileOutputStream stream;
    String sComment;
    Double dWert;
    Number nWert;
    PrintWriter pWrtr;

    pWrtr = null;
    outfile = new File( dir, "wlm_" + kurzname + ".par" );
    try
    {
      stream = new FileOutputStream( outfile );

      pWrtr = new PrintWriter( new BufferedWriter( new OutputStreamWriter( stream,
          TubigConst.TUBIG_CODEPAGE ) ) );

      sComment = (String)featPegel.getProperty( "Kommentar" );
      pWrtr.println( sComment );

      dWert = (Double)featPegel.getProperty( "XAW" );
      Format.fprintf( pWrtr, TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
      { dWert } );
      pWrtr.println( TubigConst.TUBIG_SEP + "XAW" );

      dWert = (Double)featPegel.getProperty( "XEW" );
      Format.fprintf( pWrtr, TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
      { dWert } );
      pWrtr.println( TubigConst.TUBIG_SEP + "XEW" );

      dWert = (Double)featPegel.getProperty( "FK1" );
      Format.fprintf( pWrtr, TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
      { dWert } );
      pWrtr.println( TubigConst.TUBIG_SEP + "FK1  Rueckgangskoeffient in h" );

      dWert = (Double)featPegel.getProperty( "FK2" );
      Format.fprintf( pWrtr, TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
      { dWert } );
      pWrtr.println( TubigConst.TUBIG_SEP + "FK2  Rueckgangskoeffient in h" );

      dWert = (Double)featPegel.getProperty( "NN1" );
      Format.fprintf( pWrtr, TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
      { dWert } );
      pWrtr.println( TubigConst.TUBIG_SEP + "NN1" );

      dWert = (Double)featPegel.getProperty( "NN2" );
      Format.fprintf( pWrtr, TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
      { dWert } );
      pWrtr.println( TubigConst.TUBIG_SEP + "NN2" );

      nWert = (Number)featPegel.getProperty( "Laufzeit" );
      Format.fprintf( pWrtr, TubigConst.TUBIG_INTEGER_FORMAT, new Object[]
      { nWert } );
      pWrtr.print( TubigConst.TUBIG_SEP );
      nWert = (Number)featPegel.getProperty( "LaufzeitDefault" );
      Format.fprintf( pWrtr, TubigConst.TUBIG_INTEGER_FORMAT, new Object[]
      { nWert } );
      pWrtr.println( TubigConst.TUBIG_SEP + "Laufzeit und LaufzeitDefault" );

      dWert = (Double)featPegel.getProperty( "DM1" );
      Format.fprintf( pWrtr, TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
      { dWert } );
      pWrtr.println( TubigConst.TUBIG_SEP + "DM1" );

      dWert = (Double)featPegel.getProperty( "DM2" );
      Format.fprintf( pWrtr, TubigConst.TUBIG_NUMBER_FORMAT, new Object[]
      { dWert } );
      pWrtr.println( TubigConst.TUBIG_SEP + "DM2" );

      pWrtr.close();

    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( pWrtr );
    }

  }

  public static void main( final String[] args ) throws Exception
  {
    final URL gmlurl = TubigExportParameter.class.getResource( "resources/bodemodell_test.gml" );
    //    final URL schemaurl = TubigExportParameter.class.getResource(
    // "resources/schema/bodemodell.xsd" );

    TypeRegistrySingleton.getTypeRegistry().registerTypeHandler( new ObservationLinkHandler() );

    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlurl );

    // zum Schreiben
    //    GmlSerializer.serializeWorkspace( aWriter, workspace );

    final File dir = new File( System.getProperty( "java.io.tmpdir" ) );
    writeSpeicherPars( workspace, dir );
    writePegelPars( workspace, dir );

  }
}
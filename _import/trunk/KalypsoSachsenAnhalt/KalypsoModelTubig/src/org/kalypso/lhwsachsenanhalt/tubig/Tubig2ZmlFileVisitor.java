/**
 * ---------------- FILE HEADER KALYPSO ---------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal
 * engineering Denickestraße 22 21073 Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany
 * http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: g.belger@bjoernsen.de m.schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------
 */
package org.kalypso.lhwsachsenanhalt.tubig;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.Iterator;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.io.FileVisitor;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.ObservationType;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Thül
 */
public class Tubig2ZmlFileVisitor implements FileVisitor
{
  private final File m_dirOut;

  private final String m_sFeatTyp;

  private final GMLWorkspace m_gmlWrkSpce;

  private final Map m_metaMap;

  /**
   * @param dirOut
   *          Zielverzeichnis
   * @param sFeatTyp
   *          um welchen FeatureTyp handelt es sich (Speicher, Pegel)
   * @param urlGml
   *          URL der GML-Datei
   * @param metaMap
   * 
   * @throws TubigException
   */
  public Tubig2ZmlFileVisitor( final File dirOut, final String sFeatTyp, final URL urlGml,
      final Map metaMap ) throws TubigException
  {
    m_dirOut = dirOut;
    m_sFeatTyp = sFeatTyp;
    m_metaMap = metaMap;

    try
    {
      m_gmlWrkSpce = GmlSerializer.createGMLWorkspace( urlGml );
    }
    catch( final Exception e )
    {
      throw new TubigException( "Fehler beim Laden der Modelldaten-GML", e );
    }
  }

  /**
   * @param dirIn
   *          Quellverzeichnis (in diesem Verzeichnis stehen die Dateien, die
   *          konvertiert werden sollen)
   * @param dirOut
   *          Zielverzeichnis
   * @param sFeatTyp
   *          um welchen FeatureTyp handelt es sich (Speicher, Pegel)
   * @param urlGml
   * @param metaMap
   */
  public static void writeZml( final File dirIn, final File dirOut, final String sFeatTyp,
      final URL urlGml, final Map metaMap ) throws TubigException
  {
    final Tubig2ZmlFileVisitor fleVisitor;
    fleVisitor = new Tubig2ZmlFileVisitor( dirOut, sFeatTyp, urlGml, metaMap );

    {
      try
      {
        FileUtilities.accept( dirIn, fleVisitor, true );
      }
      catch( final Exception e )
      {
        throw new TubigException( "Fehler beim Schreiben der Zeitreihen ins ZML-Format", e );
      }
    }
  }

  /**
   * @see org.kalypso.java.io.FileVisitor#visit(java.io.File)
   */
  public boolean visit( final File fleTubig )
  {
    final IObservation obsZml;
    final FileOutputStream fleOutStream;
    final File fleZml;
    final ObservationType observationType;

    OutputStreamWriter outStreamWrtr;
    ZmlInfo info;
    String name;

    outStreamWrtr = null;

    if( fleTubig.isFile() )
    {
      // ZML-Datei und Observation erzeugen
      fleZml = createFleZml( fleTubig );

      // key erzeugen
      final String sValueType = TubigUtils.getObservationType( fleTubig );
      final String sKurzName = TubigUtils.getFileNameWOExt( fleTubig );

      final String key = m_sFeatTyp + "@" + sKurzName + "@" + sValueType;
      info = (ZmlInfo)m_metaMap.get( key );

      if( info == null )
      { // Falls keine info vorhanden, dann Default-Info nehmen
        info = (ZmlInfo)m_metaMap.get( TubigConst.DEFAULT );
        if( fleZml != null )
        {
          name = TubigUtils.getFileNameWOExt( fleZml );
        }
        else
        {
          name = TubigConst.UNBEKANNT;
        }
      }
      else
      {
        name = info.getName();
      }

      obsZml = TubigConverter.tubig2Zml( fleTubig, name );

      // Metadaten in obs übertragen
      // ein bisserl tricky, damit die neuen Werte garantiert die alten
      // überschreiben
      final MetadataList metadataList = obsZml.getMetadataList();
      final MetadataList newmeta = new MetadataList();
      if( info != null )
      {
        final MetadataList oldmeta = info.getMetadata();
        newmeta.putAll( oldmeta );
      }
      newmeta.putAll( metadataList );
      metadataList.putAll( newmeta );

      try
      {
        fleOutStream = new FileOutputStream( fleZml );
        outStreamWrtr = new OutputStreamWriter( fleOutStream, TubigConst.ZML_CODEPAGE );
        observationType = ZmlFactory.createXML( obsZml, null );
        ZmlFactory.getMarshaller().marshal( observationType, outStreamWrtr );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
      finally
      {
        IOUtils.closeQuietly( outStreamWrtr );
      }
    }
    return true;
  }

  /**
   * gibt ZML-File für TUBIG-Datei zurück. Das Verzeichnis, in dem die ZML sttht
   * wird gleichzeitig angelegt
   * 
   * @param fleTubig
   */
  private File createFleZml( final File fleTubig )
  {
    final File flePathZml;
    final String sTubigExt;
    final String sKurzName;

    File dirZml;
    File fleZml;

    fleZml = null;

    sTubigExt = TubigUtils.getExtension( fleTubig );
    sKurzName = TubigUtils.getFileNameWOExt( fleTubig );

    flePathZml = getZmlPath( sKurzName, sTubigExt );
    if( flePathZml != null )
    {
      dirZml = new File( m_dirOut, flePathZml.getParent() );
      dirZml.mkdirs();
      fleZml = new File( dirZml, flePathZml.getName() );
    }
    return fleZml;
  }

  /**
   * gibt relativen Pfad (TimeSeriesLink) einer ZML aus GML zurück
   * 
   * @param sKurzName
   *          zu welchem Kurznamen wird TimeSeriesLink gesucht
   * @param sTubigExt
   *          aus der Tubig-Dateiendung wird abgeleitet, welcher TimeSeriesLink
   *          gesucht wird
   */
  private File getZmlPath( final String sKurzName, final String sTubigExt )
  {
    final FeatureList featList;
    File flePathZml;
    final Iterator itFeat;
    Feature feat;
    Object objLinkProp;

    flePathZml = null;
    objLinkProp = null;

    if( m_sFeatTyp.equals( TubigConst.PEGEL ) || m_sFeatTyp.equals( TubigConst.SPEICHER ) )
    {
      // Features lesen und daraus den ZML-Link generieren
      //(Pegel *.pq oder Speicher *.q, *.zfl, *.pv)
      if( m_sFeatTyp.equals( TubigConst.PEGEL ) )
      {
        featList = (FeatureList)m_gmlWrkSpce.getFeatureFromPath( TubigConst.GML_PEGEL_COLL );
      }
      else
      {
        featList = (FeatureList)m_gmlWrkSpce.getFeatureFromPath( TubigConst.GML_ALLE_SPEICHER_COLL );
      }

      for( itFeat = featList.iterator(); itFeat.hasNext(); )
      {
        feat = (Feature)itFeat.next();
        if( sKurzName.equals( feat.getProperty( TubigConst.GML_KURZ_NAME ) ) )
        {
          if( m_sFeatTyp.equals( TubigConst.PEGEL ) )
          {
            if( sTubigExt.equals( "pq" ) )
            {
              objLinkProp = feat.getProperty( "Ganglinie_gerechnet" );
            }
            if( sTubigExt.equals( "vq" ) )
            {
              objLinkProp = feat.getProperty( "Ganglinie_gemessen" );
            }
          }
          else if( m_sFeatTyp.equals( TubigConst.SPEICHER ) )
          {
            if( sTubigExt.equals( "q" ) )
            {
              objLinkProp = feat.getProperty( "Abgabe" );
            }
            else if( sTubigExt.equals( "zfl" ) )
            {
              objLinkProp = feat.getProperty( "Zufluss_gerechnet" );
            }
            else if( sTubigExt.equals( "pv" ) )
            {
              objLinkProp = feat.getProperty( "Ganglinie_gerechnet" );
            }
          }
          break;
        }
      }

      if( objLinkProp instanceof TimeseriesLinkType )
      {
        final TimeseriesLinkType tlt = (TimeseriesLinkType)objLinkProp;
        final String href = tlt.getHref();
        flePathZml = new File( href );
      }
    }
    return flePathZml;
  }
}
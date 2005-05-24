/*
 * ---------------- FILE HEADER KALYPSO ----------------------------------------
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
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------
 */
package org.kalypso.lhwsachsenanhalt.tubig;

import java.io.File;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Map;

import org.kalypso.java.net.UrlUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Thül
 */
public class Zml2TubigFeatureVisitor implements FeatureVisitor
{
  public static void writeTimeseries( final GMLWorkspace workspace, final String featurePath,
      final URL context, final File outdir, final String tubigProperty, final String linkProperty,
      final int step, final String ext, Date dtStartForecast, final Map metaMap,
      final String sFeatTyp ) throws TubigException
  {
    final FeatureList features = (FeatureList)workspace.getFeatureFromPath( featurePath );
    final Zml2TubigFeatureVisitor speicherVisitor = new Zml2TubigFeatureVisitor( context, outdir,
        tubigProperty, linkProperty, step, ext, dtStartForecast, metaMap, sFeatTyp );
    features.accept( speicherVisitor );

    if( speicherVisitor.hasException() )
      throw new TubigException( "Fehler beim Schreiben der Zeitreihen ins Tubig-Format",
          speicherVisitor.getExceptions()[0] );
  }

  private final String m_linkProperty;

  private final UrlUtilities m_urlresolver = new UrlUtilities();

  private final URL m_context;

  private final String m_tubigProperty;

  private final int m_step;

  private final File m_outdir;

  private final String m_ext;

  private Collection m_exceptions = new ArrayList();

  private final Date m_dtStartForecast;

  private final Map m_metaMap;

  private final String m_sFeatTyp;

  /**
   * @param ext
   *          Dateiextension ohne Punkt. Bestimmt die Achse der Observation, die
   *          rausgeschrieben wird.
   * @param dtStartForecast
   * @param metaMap
   * @param sFeatTyp
   */
  public Zml2TubigFeatureVisitor( final URL context, final File outdir, final String tubigProperty,
      final String linkProperty, final int step, final String ext, Date dtStartForecast,
      final Map metaMap, final String sFeatTyp )
  {
    m_outdir = outdir;
    m_tubigProperty = tubigProperty;
    m_linkProperty = linkProperty;
    m_context = context;
    m_step = step;
    m_ext = ext;
    m_dtStartForecast = dtStartForecast;
    m_metaMap = metaMap;
    m_sFeatTyp = sFeatTyp;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final Object property = f.getProperty( m_linkProperty );
    final String shortname = (String)f.getProperty( m_tubigProperty );
    if( property instanceof TimeseriesLinkType )
    {
      final TimeseriesLinkType tlt = (TimeseriesLinkType)property;
      final String href = tlt.getHref();

      try
      {
        final URL url = m_urlresolver.resolveURL( m_context, href );
        final URLConnection urlConTest = UrlUtilities.connectQuietly( url );

        if( urlConTest != null )
        {
          final IObservation observation = ZmlFactory.parseXML( url, f.getId() );
          final File tubigFile = new File( m_outdir, shortname + "." + m_ext );
          final String sValueType = TubigUtils.getObservationType( tubigFile );
          if( m_metaMap != null )
          {
            final ZmlInfo info = new ZmlInfo( observation.getName(), observation
                .getMetadataList() );

            createDefaultMetaInfo( m_metaMap, info );

            m_metaMap.put( m_sFeatTyp + "@" + shortname + "@" + sValueType, info );
          }
          TubigConverter.zml2Tubig( observation, tubigFile, m_step, m_dtStartForecast );
        }
      }
      catch( final Exception e )
      {
        m_exceptions.add( e );
      }

    }
    return true;
  }

  public boolean hasException()
  {
    return !m_exceptions.isEmpty();
  }

  public Exception[] getExceptions()
  {
    return (Exception[])m_exceptions.toArray( new Exception[m_exceptions.size()] );
  }

  /**
   * Wenn noch nicht vorhanden, wird ein Default-Eintrag für MetaMap generiert,
   * der die StartZeit und den Simualationszeitraum aus info holt <br>
   * In der metaMap werden die ursprünglichen Metadaten der ZMLs aufbewahrt, und
   * beim Schreiben der Ergebnisse verwendet
   */
  private static void createDefaultMetaInfo( Map metaMap, ZmlInfo info )
  {
    final MetadataList metaDataListDef;
    final MetadataList metaDataList;
    final String sNameDef;

    ZmlInfo infoDef;
    String sTempProp;

    infoDef = (ZmlInfo)metaMap.get( TubigConst.DEFAULT );
    if( infoDef == null )
    {
      sNameDef = TubigConst.UNBEKANNT;
      metaDataList = info.getMetadata();
      metaDataListDef = new MetadataList();

      metaDataListDef.setProperty( TubigConst.PROP_COMMENT, "" );
      sTempProp = metaDataList.getProperty( TubigConst.PROP_STARTZEIT );
      if( sTempProp != null )
      {
        metaDataListDef.setProperty( TubigConst.PROP_STARTZEIT, metaDataList
            .getProperty( TubigConst.PROP_STARTZEIT ) );
      }
      metaDataListDef.setProperty( TubigConst.PROP_NAME, sNameDef );
      sTempProp = metaDataList.getProperty( TubigConst.PROP_VORHERSAGE );
      if( sTempProp != null )
      {
        metaDataListDef.setProperty( TubigConst.PROP_VORHERSAGE, sTempProp );
      }

      infoDef = new ZmlInfo( sNameDef, metaDataListDef );

      metaMap.put( TubigConst.DEFAULT, infoDef );
    }
  }
}
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
package org.kalypso.lhwzsachsen.elbepolte.visitors;

import java.io.File;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.lhwzsachsen.elbepolte.ElbePolteConverter;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author thuel2
 */
public class FeatureVisitorZml2Hwvs implements FeatureVisitor
{

  /**
   * @param wks
   * @param propFeatList
   * @param propZmlLink
   *          property that holds link to zml to be converted
   * @param propId
   *          property that holds file extension
   * @param prefixFile
   *          prefix of filenames
   * @param fileDir
   *          dir to write files to
   * @throws Exception
   */
  public static void writeTimeseries( GMLWorkspace wks, String propFeatList, String propZmlLink, String propId,
      String prefixFile, File fileDir, URL context ) throws Exception
  {

    final FeatureList features = (FeatureList)wks.getFeatureFromPath( propFeatList );

    final FeatureVisitorZml2Hwvs featVisitor = new FeatureVisitorZml2Hwvs( propZmlLink, propId, prefixFile, fileDir,
        context );

    features.accept( featVisitor );

    if( featVisitor.hasException() )
      throw new Exception( "Fehler beim Schreiben der Zeitreihen ins Modell-Format: "
          + featVisitor.getExceptions()[0].getLocalizedMessage(), featVisitor.getExceptions()[0] );

  }

  private final String m_propZmlLink;
  private final String m_propId;
  private final String m_prefixFile;
  private final File m_fileDir;
  private final URL m_context;
  private final UrlUtilities m_urlresolver = new UrlUtilities();
  private Collection m_exceptions = new ArrayList();

  /**
   * @param propZmlLink
   * @param propId
   * @param prefixFile
   * @param fileDir
   * @param context
   *          TODO
   *  
   */

  public FeatureVisitorZml2Hwvs( String propZmlLink, String propId, String prefixFile, File fileDir, final URL context )
  {
    m_propZmlLink = propZmlLink;
    m_propId = propId;
    m_prefixFile = prefixFile;
    m_fileDir = fileDir;
    m_context = context;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( Feature f )
  {
    // link auf ZML und dann ZML selbst holen und öffnen
    final Object objZmlProp = f.getProperty( m_propZmlLink );
    final String sFileNme = m_prefixFile + "." + (String)f.getProperty( m_propId );
    if( objZmlProp instanceof TimeseriesLinkType )
    {
      final TimeseriesLinkType tlt = (TimeseriesLinkType)objZmlProp;
      final String href = tlt.getHref();

      final URL url;
      try
      {
        url = m_urlresolver.resolveURL( m_context, href );

        final URLConnection urlConTest = UrlUtilities.connectQuietly( url );

        if( urlConTest != null )
        {
          final IObservation observation = ZmlFactory.parseXML( url, f.getId() );
          //        Ziel-Datei erstellen
          final File fleHwvs = new File( m_fileDir, sFileNme );
          // Zml2Hwvs ausführen
          // TODO Zeitraum wäre noch schön
          final String sComment = fleHwvs.getName() + ": " +observation.getIdentifier() ; 
          ElbePolteConverter.zml2Hwvs( observation, fleHwvs, sComment );
        }
        else
        {
          m_exceptions.add( new Exception( "Zeitreihe " + href.toString()
              + " kann nicht geöffnet werden (da vielleicht nicht vorhanden)." ) );
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

}

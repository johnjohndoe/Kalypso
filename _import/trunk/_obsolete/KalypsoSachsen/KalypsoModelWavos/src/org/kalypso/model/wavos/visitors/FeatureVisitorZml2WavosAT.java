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
package org.kalypso.model.wavos.visitors;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.logging.Logger;

import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.model.wavos.WavosConverter;
import org.kalypso.model.wavos.WavosInputWorker;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author  thuel2
 */
public class FeatureVisitorZml2WavosAT implements FeatureVisitor
{
  private final static Logger m_logger = Logger.getLogger( WavosInputWorker.class.getName() );

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( Feature f )
  {
    // link auf ZML und dann ZML selbst holen und öffnen
    final Object objZmlProp = f.getProperty( m_propZmlLink );
    final String sFileNme = (String)f.getProperty( m_propId ) + ".at";
    final Object property = f.getProperty( m_createAt );
    final boolean blnWriteAT = property == null? false: ( (Boolean)property ).booleanValue();
    if( blnWriteAT && objZmlProp instanceof TimeseriesLinkType )
    {
      final TimeseriesLinkType tlt = (TimeseriesLinkType)objZmlProp;
      final String href = tlt.getHref();


      final URL url;
      try
      {
        url = m_urlresolver.resolveURL( m_context, href );
        WavosConverter.zml2At( url, m_tafelDir, sFileNme, f.getId(), true );
      }
      catch( final Exception e )
      {
        m_exceptions.add( e );
      }
    }
  

    return true;
  }

  /**
   * @param wks
   * @param tafelDir
   */
  public static void writeAT( final GMLWorkspace wks, final File tafelDir, final URL context )
      throws Exception
  {
    final FeatureList features = (FeatureList)wks.getFeatureFromPath( "pegelMember" );

    final FeatureVisitorZml2WavosAT featVisitor = new FeatureVisitorZml2WavosAT( tafelDir, context );

    features.accept( featVisitor );

    if( featVisitor.hasException() )
    {
      m_logger.info( "Fehler beim Schreiben der W/Q-Beziehung ins Modell-Format (AT-Datei): "
          + featVisitor.getExceptions()[0].getLocalizedMessage() );
      throw new Exception( "Fehler beim Schreiben der W/Q-Beziehung ins Modell-Format (AT-Datei): "
          + featVisitor.getExceptions()[0].getLocalizedMessage(), featVisitor.getExceptions()[0] );
    }
  }

  private final String m_propZmlLink = "ganglinie_gesamt";
  private final String m_propId = "kenn";
  private final String m_createAt = "createAT";

  private final File m_tafelDir;
  private final URL m_context;

private final UrlUtilities m_urlresolver = new UrlUtilities();
  private Collection m_exceptions = new ArrayList();

  public FeatureVisitorZml2WavosAT( final File tafelDir, final URL context )
  {
    m_tafelDir = tafelDir;
    m_context = context;
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

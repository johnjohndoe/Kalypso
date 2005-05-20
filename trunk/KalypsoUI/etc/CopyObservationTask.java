/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.util.ant;

import java.io.BufferedWriter;
import java.io.StringWriter;
import java.net.URL;
import java.util.LinkedList;
import java.util.List;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.kalypso.ogc.util.CopyObservationHandler;
import org.kalypso.ogc.util.CopyObservationHandler.Source;
import org.kalypso.util.url.UrlResolver;

/**
 * Ein Ant Task, der Zeitreihen-Links in GMLs kopiert. Die generelle Idee ist
 * es, alle Features eines GML durchzugehen, und f�r jedes Feature eine
 * Zeitreihe (definiert �ber einen Link) zu lesen und an eine andere Stelle
 * (definiert durch eine andere Property des Features) zu schreiben.
 * 
 * <code>
 *    <copyObservation gml="${project.dir}/.templates/Modell/wiskiimport_durchflu�.gml" featurePath="Wiski" context="${calc.dir}" targetObservation="lokal">
 *      <source property="wiski_vergangenheit" from="${startsim}" to="${stopsim}" />
 *    </copyObservation>
 * </code>
 * 
 * @author belger
 */
public class CopyObservationTask extends Task
{
  /** href auf das GML */
  private String m_gml;
  
  /** Feature-Path innerhalb des GMLs. Alle durch diesen Pfad denotierten Features werden behandelt. */
  private String m_featurePath;
  
  /** Kontext (=URL), gegen welche die Links innerhalb des GML aufgel�st werden. */
  private URL m_context;
  
  /** Name der Feature-Property, welche den Link enth�lt, an welche Stelle das Ergebnis geschrieben wird. */
  private String m_targetobservation;

  /** Ordered List of 'Source' Elements. Each source will be read as Observation, the combination of all sources will be written to 'targetobservation' */
  private List m_sources = new LinkedList( );

  /**
   * @see org.apache.tools.ant.Task#execute()
   */
  public void execute() throws BuildException
  {
    try
    {
      final UrlResolver urlResolver = new UrlResolver();
      final URL gmlURL = urlResolver.resolveURL( m_context, getGml() );
      
      final StringWriter detailWriter = new StringWriter( );
      final BufferedWriter detailBW = new BufferedWriter( detailWriter );
      final StringWriter summaryWriter = new StringWriter( );
      final BufferedWriter summaryBW = new BufferedWriter( summaryWriter );

      CopyObservationHandler.copyObserations( urlResolver, gmlURL, getFeaturePath(), getTargetobservation(), getContext(), (Source[])m_sources
          .toArray( new CopyObservationHandler.Source[m_sources.size()] ), summaryBW, detailBW );

      summaryBW.close();
      detailBW.close();

      log( "Zusammenfassung" );
      log( summaryWriter.toString() );

      log( "\nDetails" );
      log( detailWriter.toString() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new BuildException( e.getLocalizedMessage(), e );
    }
  }
  
  public final URL getContext()
  {
    return m_context;
  }
  public final void setContext( final URL context )
  {
    m_context = context;
  }
  public final String getFeaturePath()
  {
    return m_featurePath;
  }
  public final void setFeaturePath( String featurePath )
  {
    m_featurePath = featurePath;
  }
  public final String getGml()
  {
    return m_gml;
  }
  public final void setGml( String gml )
  {
    m_gml = gml;
  }
  public final String getTargetobservation()
  {
    return m_targetobservation;
  }
  public final void setTargetobservation( final String targetobservation )
  {
    m_targetobservation = targetobservation;
  }
  
  public void addSource( final CopyObservationHandler.Source source )
  {
    m_sources.add( source );
  }
}

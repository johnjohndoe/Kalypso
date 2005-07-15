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
package org.kalypso.ant;

import java.io.BufferedWriter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.ogc.util.CopyObservationHandler;

/**
 * Ein Ant Task, der Zeitreihen-Links in GMLs kopiert. Die generelle Idee ist es, alle Features eines GML durchzugehen,
 * und für jedes Feature eine Zeitreihe (definiert über einen Link) zu lesen und an eine andere Stelle (definiert durch
 * eine andere Property des Features) zu schreiben.
 * 
 * <code>
 *    <copyObservation gml="${project.dir}/.templates/Modell/wiskiimport_durchfluß.gml" featurePath="Wiski" context="${calc.dir}" targetObservation="lokal">
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

  /**
   * Feature-Path innerhalb des GMLs. Alle durch diesen Pfad denotierten Features werden behandelt.
   */
  private String m_featurePath;

  /** Kontext (=URL), gegen welche die Links innerhalb des GML aufgelöst werden. */
  private URL m_context;

  /**
   * Name der Feature-Property, welche den Link enthält, an welche Stelle das Ergebnis geschrieben wird.
   */
  private String m_targetobservation;

  /**
   * Wir benutzt, um den entsprechenden Metadata-Eintrag in den Zeitreiehen zu generieren Default mit -1 damit getestet
   * werden kann ob die Eigenschaft gesetzt wurde.
   */
  private long m_forecastFrom = -1;

  /**
   * Wir benutzt, um den entsprechenden Metadata-Eintrag in den Zeitreiehen zu generieren Default mit -1 damit getestet
   * werden kann ob die Eigenschaft gesetzt wurde.
   */
  private long m_forecastTo = -1;

  /**
   * Ordered List of 'Source' Elements. Each source will be read as Observation, the combination of all sources will be
   * written to 'targetobservation'
   */
  private List m_sources = new LinkedList();

  /**
   * List of metadata-properties and values to set to the target observation
   */
  private Properties m_metadata = new Properties();

  /**
   * @see org.apache.tools.ant.Task#execute()
   */
  public void execute() throws BuildException
  {
    try
    {
      final UrlResolver urlResolver = new UrlResolver();
      final URL gmlURL = urlResolver.resolveURL( m_context, getGml() );

      final StringWriter logwriter = new StringWriter();
      final PrintWriter logPW = new PrintWriter( new BufferedWriter( logwriter ) );

      Date forecastFrom = null;
      if( m_forecastFrom != -1 )
        forecastFrom = new Date( m_forecastFrom );

      Date forecastTo = null;
      if( m_forecastTo != -1 )
        forecastTo = new Date( m_forecastTo );

      final CopyObservationHandler.Source[] srcs = (CopyObservationHandler.Source[])m_sources
          .toArray( new CopyObservationHandler.Source[m_sources.size()] );
      CopyObservationHandler.copyObserations( urlResolver, gmlURL, getFeaturePath(), getTargetobservation(),
          getContext(), srcs, m_metadata, forecastFrom, forecastTo, logPW );

      logPW.close();

      log( logwriter.toString() );
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

  public void addConfiguredSource( final Source source )
  {
    // validate source
    final String property = source.getProperty();
    final long from = source.getFrom();
    final long to = source.getTo();

    final Date fromDate = new Date( from );
    final Date toDate = new Date( to );

    getProject().log(
        "Adding source: property=" + property + ", from=" + fromDate.toString() + ", to=" + toDate.toString(),
        Project.MSG_DEBUG );

    m_sources.add( new CopyObservationHandler.Source( property, fromDate, toDate ) );
  }

  public void addConfiguredMetadata( final Metadata metadata )
  {
    if( metadata.getName() == null )
    {
      getProject().log( "Cannot add Metadata since property name is null", Project.MSG_WARN );
      return;
    }

    if( metadata.getValue() == null )
    {
      getProject().log( "Cannot add Metadata since property value is null. Property name: " + metadata.getName(),
          Project.MSG_WARN );
      return;
    }

    m_metadata.setProperty( metadata.getName(), metadata.getValue() );
  }

  public final static class Source
  {
    private String property;

    private long from;

    private long to;

    public final String getProperty()
    {
      return property;
    }

    public final void setProperty( String prop )
    {
      this.property = prop;
    }

    public final long getFrom()
    {
      return from;
    }

    public final void setFrom( long lfrom )
    {
      this.from = lfrom;
    }

    public final long getTo()
    {
      return to;
    }

    public final void setTo( long lto )
    {
      this.to = lto;
    }
  }

  public final long getForecastFrom()
  {
    return m_forecastFrom;
  }

  public final void setForecastFrom( long forecastFrom )
  {
    m_forecastFrom = forecastFrom;
  }

  public final long getForecastTo()
  {
    return m_forecastTo;
  }

  public final void setForecastTo( long forecastTo )
  {
    m_forecastTo = forecastTo;
  }

  public final static class Metadata
  {
    private String m_name;
    private String m_value;

    public String getName()
    {
      return m_name;
    }

    public void setName( String name )
    {
      m_name = name;
    }

    public String getValue()
    {
      return m_value;
    }

    public void setValue( String value )
    {
      m_value = value;
    }
  }
}
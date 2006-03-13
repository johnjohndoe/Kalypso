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

import java.io.File;
import java.io.PrintWriter;
import java.net.URL;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;

import org.apache.tools.ant.Project;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.ogc.util.CopyObservationFeatureVisitor;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * Ein Ant Task, der Zeitreihen-Links in GMLs kopiert. Die generelle Idee ist es, alle Features eines GML durchzugehen,
 * und f�r jedes Feature eine Zeitreihe (definiert �ber einen Link) zu lesen und an eine andere Stelle (definiert durch
 * eine andere Property des Features) zu schreiben. <code>
 *    <copyObservation gml="${project.dir}/.templates/Modell/wiskiimport_durchflu�.gml" featurePath="Wiski" context="${calc.dir}" targetObservation="lokal">
 *      <source property="wiski_vergangenheit" from="${startsim}" to="${stopsim}" />
 *    </copyObservation>
 * </code>
 * 
 * @author belger
 */
public class CopyObservationTask extends AbstractFeatureVisitorTask
{
  /**
   * Zielverzeichnis f�r generierte Zeitreihen. �berschreibt targetobservation.
   */
  private File m_targetObservationDir;

  /**
   * Name der Feature-Property, welche den Link enth�lt, an welche Stelle das Ergebnis geschrieben wird.
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
   * Die Liste der Tokens und deren Ersetzung in der Form:
   * <p>
   * tokenName-featurePropertyName;tokenName-featurePropertyName;...
   * <p>
   * Die werden benutzt um token-replace im Zml-Href durchzuf�hren (z.B. um automatisch der Name der Feature als
   * Request-Name zu setzen)
   */
  private String m_tokens = "";

  /**
   * Ordered List of 'Source' Elements. Each source will be read as Observation, the combination of all sources will be
   * written to 'targetobservation'
   */
  private List<CopyObservationFeatureVisitor.Source> m_sources = new LinkedList<CopyObservationFeatureVisitor.Source>();

  /**
   * List of metadata-properties and values to set to the target observation
   */
  private Properties m_metadata = new Properties();

  public CopyObservationTask( )
  {
    super( false );
  }

  /**
   * @see org.kalypso.ant.AbstractFeatureVisitorTask#createVisitor(java.net.URL,
   *      org.kalypso.contribs.java.net.IUrlResolver, java.io.PrintWriter, org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected final FeatureVisitor createVisitor( final URL context, final IUrlResolver resolver, final PrintWriter logWriter, final IProgressMonitor monitor )
  {
    Date forecastFrom = null;
    if( m_forecastFrom != -1 )
      forecastFrom = new Date( m_forecastFrom );

    Date forecastTo = null;
    if( m_forecastTo != -1 )
      forecastTo = new Date( m_forecastTo );

    final CopyObservationFeatureVisitor.Source[] srcs = m_sources.toArray( new CopyObservationFeatureVisitor.Source[m_sources.size()] );
    if( m_targetObservationDir != null )
    {
      return new CopyObservationFeatureVisitor( context, resolver, m_targetObservationDir, srcs, m_metadata, forecastFrom, forecastTo, logWriter, m_tokens );

    }
    return new CopyObservationFeatureVisitor( context, resolver, m_targetobservation, srcs, m_metadata, forecastFrom, forecastTo, logWriter, m_tokens );
  }

  public final String getTargetobservation( )
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
    final String filter = source.getFilter();
    final Project project2 = getProject();
    if(project2!=null)
      project2.log( "Adding source: property=" + property + ", from=" + fromDate.toString() + ", to=" + toDate.toString(), Project.MSG_DEBUG );

    m_sources.add( new CopyObservationFeatureVisitor.Source( property, fromDate, toDate, filter ) );
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
      getProject().log( "Cannot add Metadata since property value is null. Property name: " + metadata.getName(), Project.MSG_WARN );
      return;
    }

    m_metadata.setProperty( metadata.getName(), metadata.getValue() );
  }

  public final static class Source
  {
    private String property;

    private long from;

    private long to;

    private String filter;

    public final String getProperty( )
    {
      return property;
    }

    public final void setProperty( String prop )
    {
      this.property = prop;
    }

    public final long getFrom( )
    {
      return from;
    }

    public final void setFrom( long lfrom )
    {
      this.from = lfrom;
    }

    public final long getTo( )
    {
      return to;
    }

    public final void setTo( long lto )
    {
      this.to = lto;
    }

    public final String getFilter( )
    {
      return filter;
    }

    public final void setFilter( final String filt )
    {
      this.filter = filt;
    }
  }

  public final long getForecastFrom( )
  {
    return m_forecastFrom;
  }

  public final void setForecastFrom( long forecastFrom )
  {
    m_forecastFrom = forecastFrom;
  }

  public final long getForecastTo( )
  {
    return m_forecastTo;
  }

  public final void setForecastTo( long forecastTo )
  {
    m_forecastTo = forecastTo;
  }

  public String getTokens( )
  {
    return m_tokens;
  }

  public void setTokens( String tokens )
  {
    m_tokens = tokens;
  }

  public final static class Metadata
  {
    private String m_name;

    private String m_value;

    public String getName( )
    {
      return m_name;
    }

    public void setName( String name )
    {
      m_name = name;
    }

    public String getValue( )
    {
      return m_value;
    }

    public void setValue( String value )
    {
      m_value = value;
    }
  }

  /**
   * @see org.kalypso.ant.AbstractFeatureVisitorTask#validateInput()
   */
  @Override
  protected void validateInput( )
  {
    // nothing to do
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.IErrorHandler#handleError(org.eclipse.swt.widgets.Shell,
   *      org.eclipse.core.runtime.IStatus)
   */
  public void handleError( final Shell shell, final IStatus status )
  {
    ErrorDialog.openError( shell, ClassUtilities.getOnlyClassName( getClass() ), "Fehler beim Kopieren der Zeitreihen", status );
  }

  public File getTargetObservationDir( )
  {
    return m_targetObservationDir;
  }

  public void setTargetObservationDir( File targetObservationDir )
  {
    m_targetObservationDir = targetObservationDir;
  }
}
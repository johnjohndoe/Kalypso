/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.model.wspm.tuhh.core.gml.CalculationWspmTuhhSteadyState;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;

import de.openali.odysseus.chart.factory.config.ChartConfigurationLoader;
import de.openali.odysseus.chart.factory.layer.Layers;
import de.openali.odysseus.chart.factory.provider.Providers;
import de.openali.odysseus.chartconfig.x020.AlignmentType;
import de.openali.odysseus.chartconfig.x020.AxisType;
import de.openali.odysseus.chartconfig.x020.AxisType.Direction;
import de.openali.odysseus.chartconfig.x020.ChartType;
import de.openali.odysseus.chartconfig.x020.LayerType;
import de.openali.odysseus.chartconfig.x020.ProviderType;
import de.openali.odysseus.chartconfig.x020.TitleType;

/**
 * @author Gernot Belger
 */
public class ResultLSChartFile extends AbstractResultLSFile
{
  static final String TOKEN_GMLFILENAME = "%GMLFILENAME%"; //$NON-NLS-1$

  private final String m_dataFilename;

  private final boolean m_isDirectionUpstreams;

  private final String m_chartTitle;

  private final TuhhCalculation m_calculation;

  public ResultLSChartFile( final File outDir, final String runoffName, final boolean isDirectionUpstreams, final String dataFilename, final String chartTitle, final TuhhCalculation calculation )
  {
    super( outDir, runoffName );

    m_isDirectionUpstreams = isDirectionUpstreams;
    m_dataFilename = dataFilename;
    m_chartTitle = chartTitle;
    m_calculation = calculation;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.schema.simulation.IResultLSFile#getTitle()
   */
  @Override
  public String getTitle( )
  {
    return Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.26" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.schema.simulation.IResultLSFile#getFilename()
   */
  @Override
  public String getFilename( )
  {
    return "L�ngsschnitt" + getRunoffName() + ".kod"; //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.schema.simulation.IResultLSFile#getResultID()
   */
  @Override
  public String getResultID( )
  {
    return "LengthSectionDiag"; //$NON-NLS-1$ 
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.schema.simulation.AbstractResultLSFile#doWrite(java.io.File)
   */
  @Override
  protected void doWrite( final File outputFile ) throws Exception
  {
    /* We just load the template and tweak the direction of the station-axis */
    final URL kodResource = ResultLengthSection.class.getResource( "resources/lengthSection.kod" ); //$NON-NLS-1$
    final String kodContent = UrlUtilities.toString( kodResource, "UTF-8" ); //$NON-NLS-1$
    final String kodContentReplaced = kodContent.replaceAll( TOKEN_GMLFILENAME, m_dataFilename );

    final InputStream inputStream = IOUtils.toInputStream( kodContentReplaced, "UTF-8" ); //$NON-NLS-1$
    final ChartConfigurationLoader ccl = new ChartConfigurationLoader( inputStream );

    final ChartType[] charts = ccl.getCharts();

    // only use first chart - there should only be one
    final ChartType chart = charts[0];

    final TitleType t1 = TitleType.Factory.newInstance();
    t1.setHorizontalTextAnchor( AlignmentType.CENTER );
    t1.setInsetBottom( 5 );
    t1.setStringValue( String.format( Messages.getString( "ResultLengthSection.2" ), m_chartTitle ) ); //$NON-NLS-1$
    chart.setTitleArray( new TitleType[] { t1 } ); //$NON-NLS-1$

    updateFixationLayer( chart );
    updateAxes( chart );

    ccl.getChartConfigurationDocument().save( outputFile );
  }

  private void updateFixationLayer( final ChartType chart )
  {
    final LayerType[] layers = findFixationLayers( chart );

    final Feature fixation = getWaterLevelFixationMember();

    for( final LayerType layer : layers )
    {
      if( Objects.isNull( fixation ) )
        Layers.remove( chart.getLayers(), layer );
      else
      {
        final ProviderType provider = layer.getProvider();
        Providers.updateParameter( provider, "observationId", fixation.getId() ); //$NON-NLS-1$
      }
    }
  }

  private Feature getWaterLevelFixationMember( )
  {
    if( !(m_calculation instanceof CalculationWspmTuhhSteadyState) )
      return null;

    final CalculationWspmTuhhSteadyState calc = (CalculationWspmTuhhSteadyState) m_calculation;
    return calc.getLinkedWaterLevelFixation();
  }

  private LayerType[] findFixationLayers( final ChartType chart )
  {
    final Set<LayerType> found = new LinkedHashSet<LayerType>();

    final LayerType[] layers = chart.getLayers().getLayerArray();
    for( final LayerType layer : layers )
    {
      if( "WspFixation".equals( layer.getId() ) ) //$NON-NLS-1$
      {
        found.add( layer );
      }
    }

    return found.toArray( new LayerType[] {} );
  }

  private void updateAxes( final ChartType chart )
  {
    final AxisType[] axes = chart.getMappers().getAxisArray();
    for( final AxisType axis : axes )
    {

      if( "Station_Axis".equals( axis.getId() ) )
      {
        if( m_isDirectionUpstreams )
          axis.setDirection( Direction.NEGATIVE );
        else
          axis.setDirection( Direction.POSITIVE );
      }
    }
  }

}

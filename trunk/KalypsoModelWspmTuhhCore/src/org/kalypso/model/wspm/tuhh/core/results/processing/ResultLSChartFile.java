/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.model.wspm.tuhh.core.results.processing;

import java.io.File;
import java.io.FileWriter;
import java.io.InputStream;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.tuhh.core.gml.CalculationWspmTuhhSteadyState;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;

import com.google.common.base.Charsets;

/**
 * @author Gernot Belger
 */
public class ResultLSChartFile extends AbstractResultLSFile
{
  static final String TOKEN_GMLFILENAME = "%GMLFILENAME%"; //$NON-NLS-1$

  static final String TOKEN_ROOT_ID = "%ROOT_ID%"; //$NON-NLS-1$

  static final String TOKEN_MODELL_GML = "%MODELL_GML%"; //$NON-NLS-1$

  static final String TOKEN_MODELL_OBS_ID = "%MODELL_OBS_ID%"; //$NON-NLS-1$

  static final String TOKEN_MODELL_CHART_TITLE = "%CHART_TITLE%"; //$NON-NLS-1$

  static final String TOKEN_MODELL_AXIS_DIRECTION = "%AXIS_DIRECTION%"; //$NON-NLS-1$

  private final String m_dataFilename;

  private final String m_isDirectionUpstreams;

  private final String m_chartTitle;

  private final TuhhCalculation m_calculation;

  private final String m_rootId;

  public ResultLSChartFile( final File outDir, final String runoffName, final boolean isDirectionUpstreams, final String dataFilename, final String chartTitle, final TuhhCalculation calculation, final String rootId )
  {
    super( outDir, runoffName );

    m_isDirectionUpstreams = isDirectionUpstreams ? "NEGATIVE" : "POSITIVE"; //$NON-NLS-1$ //$NON-NLS-2$
    m_dataFilename = dataFilename;
    m_chartTitle = chartTitle;
    m_calculation = calculation;
    m_rootId = rootId;
  }

  @Override
  public String getTitle( )
  {
    return Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.WspmTuhhCalcJob.26" ); //$NON-NLS-1$
  }

  @Override
  public String getFilename( )
  {
    return "L‰ngsschnitt" + getRunoffName() + ".kod"; //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.schema.simulation.IResultLSFile#getResultID()
   */
  @Override
  public String getResultID( )
  {
    return "LengthSectionDiag"; //$NON-NLS-1$
  }

  @Override
  protected void doWrite( final File outputFile ) throws Exception
  {
    final URL kodResource = getClass().getResource( "resources/lengthSection.kod" ); //$NON-NLS-1$
    final String kodContent = IOUtils.toString( kodResource, Charsets.UTF_8 );
    String kodContentReplaced = kodContent.replaceAll( TOKEN_GMLFILENAME, m_dataFilename );
    kodContentReplaced = kodContentReplaced.replaceAll( TOKEN_ROOT_ID, m_rootId );
    kodContentReplaced = kodContentReplaced.replaceAll( TOKEN_MODELL_GML, "./../../../modell.gml" ); //$NON-NLS-1$
    kodContentReplaced = kodContentReplaced.replaceAll( TOKEN_MODELL_CHART_TITLE, String.format( Messages.getString( "ResultLengthSection.2" ), m_chartTitle ) ); //$NON-NLS-1$
    kodContentReplaced = kodContentReplaced.replaceAll( TOKEN_MODELL_AXIS_DIRECTION, m_isDirectionUpstreams );
    final Feature fixation = getWaterLevelFixationMember();
    kodContentReplaced = kodContentReplaced.replaceAll( TOKEN_MODELL_OBS_ID, Objects.isNull( fixation ) ? "" : fixation.getId() ); //$NON-NLS-1$

    final FileWriter writer =new FileWriter( outputFile);
    final InputStream inputStream = IOUtils.toInputStream( kodContentReplaced, "UTF-8" ); //$NON-NLS-1$
    IOUtils.copy( inputStream, writer );
    IOUtils.closeQuietly( writer );
//    writer.write( kodContentReplaced );
//    writer.close();
//   // final InputStream inputStream = IOUtils.toInputStream( kodContentReplaced, "UTF-8" ); //$NON-NLS-1$
//
//    final ChartConfigurationLoader ccl = new ChartConfigurationLoader( inputStream );
//
//    final ChartType[] charts = ccl.getCharts();
//
//    // only use first chart - there should only be one
//    final ChartType chart = charts[0];
//
//    final TitleType t1 = TitleType.Factory.newInstance();
//    t1.setHorizontalTextAnchor( AlignmentType.CENTER );
//    t1.setInsetBottom( 5 );
//    t1.setStringValue( String.format( Messages.getString( "ResultLengthSection.2" ), m_chartTitle ) ); //$NON-NLS-1$
//    chart.setTitleArray( new TitleType[] { t1 } ); //$NON-NLS-1$
//
//    // updateFixationLayer( chart );
//    // updateAxes( chart );
//
//    ccl.getChartConfigurationDocument().save( outputFile );
  }

// private void updateFixationLayer( final ChartType chart )
// {
// final LayerType[] layers = findFixationLayers( chart );
//
// final Feature fixation = getWaterLevelFixationMember();
//
// for( final LayerType layer : layers )
// {
// if( Objects.isNull( fixation ) )
// Layers.remove( chart.getLayers(), layer );
// else
// {
// final ProviderType provider = layer.getProvider();
//        Providers.updateParameter( provider, "observationId", fixation.getId() ); //$NON-NLS-1$
// }
// }
// }

  private Feature getWaterLevelFixationMember( )
  {
    if( !(m_calculation instanceof CalculationWspmTuhhSteadyState) )
      return null;

    final CalculationWspmTuhhSteadyState calc = (CalculationWspmTuhhSteadyState) m_calculation;
    return calc.getLinkedWaterLevelFixation();
  }

// private LayerType[] findFixationLayers( final ChartType chart )
// {
// final Set<LayerType> found = new LinkedHashSet<LayerType>();
//
// final LayerType[] layers = chart.getLayers().getLayerArray();
// for( final LayerType layer : layers )
// {
//      if( "WspFixation".equals( layer.getId() ) ) //$NON-NLS-1$
// {
// found.add( layer );
// }
// }
//
// return found.toArray( new LayerType[] {} );
// }

// private void updateAxes( final ChartType chart )
// {
// final AxisType[] axes = chart.getMappers().getAxisArray();
// for( final AxisType axis : axes )
// {
//
//      if( "Station_Axis".equals( axis.getId() ) ) //$NON-NLS-1$
// {
// if( m_isDirectionUpstreams )
// axis.setDirection( Direction.NEGATIVE );
// else
// axis.setDirection( Direction.POSITIVE );
// }
// }
// }
}
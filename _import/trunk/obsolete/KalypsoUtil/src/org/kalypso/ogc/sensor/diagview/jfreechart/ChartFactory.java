package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.io.IOException;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;
import java.util.Vector;

import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.StandardXYItemRenderer;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.ICurve;
import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.util.factory.ConfigurableCachableObjectFactory;
import org.kalypso.util.factory.FactoryException;

/**
 * @author schlienger
 */
public class ChartFactory
{
  private final static ConfigurableCachableObjectFactory m_objFactory;

  static
  {
    Properties props = new Properties();
    try
    {
      props.load( ChartFactory.class.getResourceAsStream( "resource/type2valueAxis.properties" ) );
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    m_objFactory = new ConfigurableCachableObjectFactory( props, false, ChartFactory.class
        .getClassLoader() );
  }

  /**
   * Creates and returns an observation chart.
   * 
   * @throws FactoryException
   */
  public static JFreeChart createObservationChart( final IDiagramTemplate template )
      throws FactoryException
  {
    XYPlot plot = new XYPlot();

    IDiagramAxis[] diagAxes = template.getAxisList();

    Map diag2chartAxis = new Hashtable( diagAxes.length );
    Vector chartAxes = new Vector( diagAxes.length );

    for( int i = 0; i < diagAxes.length; i++ )
    {
      ValueAxis va = prepareChartAxis( i, diagAxes[i], plot );

      diag2chartAxis.put( diagAxes[i], va );
      chartAxes.add( va );
    }

    ICurve[] curves = template.getCurveList();
    for( int i = 0; i < curves.length; i++ )
    {
      try
      {
        CurveDataset cds = new CurveDataset( curves[i] );

        plot.setDataset( i, cds );

        plot.mapDatasetToDomainAxis( i, chartAxes.indexOf( diag2chartAxis.get( cds.getXAxis() ) ) );
        plot.mapDatasetToRangeAxis( i, chartAxes.indexOf( diag2chartAxis.get( cds.getYAxis() ) ) );
      }
      catch( SensorException e )
      {
        throw new FactoryException( e );
      }
    }

    plot.setRenderer( new StandardXYItemRenderer( StandardXYItemRenderer.LINES ) );
    JFreeChart chart = new JFreeChart( template.getTitle(), JFreeChart.DEFAULT_TITLE_FONT, plot,
        template.isShowLegend() );

    return chart;
  }

  /**
   * @throws FactoryException
   */
  private static ValueAxis prepareChartAxis( final int pos, final IDiagramAxis diagAxis,
      final XYPlot plot ) throws FactoryException
  {
    ValueAxis vAxis = (ValueAxis)m_objFactory.getObjectInstance( diagAxis.getDataType(),
        ValueAxis.class, new Object[]
        { diagAxis.getLabel() + " [" + diagAxis.getUnit() + "]" } );

    vAxis.setInverted( diagAxis.isInverted() );
    vAxis.setLowerMargin( 0.02 );
    vAxis.setUpperMargin( 0.02 );

    AxisLocation loc = getLocation( diagAxis );

    if( diagAxis.getDirection().equals( IDiagramAxis.DIRECTION_HORIZONTAL ) )
    {
      plot.setDomainAxis( pos, vAxis );
      plot.setDomainAxisLocation( pos, loc );
    }
    else
    {
      plot.setRangeAxis( pos, vAxis );
      plot.setRangeAxisLocation( pos, loc );
    }

    return vAxis;
  }

  /**
   *  
   */
  private static AxisLocation getLocation( IDiagramAxis diagAxis )
  {
    if( diagAxis.getPosition().equals( IDiagramAxis.POSITION_BOTTOM ) )
    {
      if( diagAxis.getPosition().equals( IDiagramAxis.POSITION_LEFT ) )
        return AxisLocation.BOTTOM_OR_LEFT;
      else if( diagAxis.getPosition().equals( IDiagramAxis.POSITION_RIGHT ) )
        return AxisLocation.BOTTOM_OR_RIGHT;
    }
    else if( diagAxis.getPosition().equals( IDiagramAxis.POSITION_TOP ) )
    {
      if( diagAxis.getPosition().equals( IDiagramAxis.POSITION_LEFT ) )
        return AxisLocation.TOP_OR_LEFT;
      else if( diagAxis.getPosition().equals( IDiagramAxis.POSITION_RIGHT ) )
        return AxisLocation.TOP_OR_RIGHT;
    }

    // default
    return AxisLocation.BOTTOM_OR_LEFT;
  }
}
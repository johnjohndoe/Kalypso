package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.Plot;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.util.factory.ConfigurableCachableObjectFactory;
import org.kalypso.util.factory.FactoryException;

/**
 * @author schlienger
 */
public final class ChartFactory
{
  private final static ConfigurableCachableObjectFactory m_objFactory;

  static
  {
    final Properties props = new Properties();
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
   * @param template
   * @return plot
   * 
   * @throws FactoryException
   */
  static Plot createObservationPlot( final IDiagramTemplate template ) throws FactoryException
  {
    final IDiagramAxis[] diagAxes = template.getAxisList();

    final Map diag2chartAxis = new HashMap( diagAxes.length );
    final Map chartAxes2Pos = new HashMap( diagAxes.length );
    
    final ObservationPlot plot = new ObservationPlot( diag2chartAxis, chartAxes2Pos );
    
    int domPos = 0;
    int ranPos = 0;

    for( int i = 0; i < diagAxes.length; i++ )
    {
      ValueAxis vAxis = (ValueAxis)m_objFactory.getObjectInstance( diagAxes[i].getDataType(),
          ValueAxis.class, new Object[]
          { diagAxes[i].toFullString() } );

      vAxis.setInverted( diagAxes[i].isInverted() );
      vAxis.setLowerMargin( 0.02 );
      vAxis.setUpperMargin( 0.02 );

      AxisLocation loc = getLocation( diagAxes[i] );

      if( diagAxes[i].getDirection().equals( IDiagramAxis.DIRECTION_HORIZONTAL ) )
      {
        plot.setDomainAxis( domPos, vAxis );
        plot.setDomainAxisLocation( domPos, loc );

        chartAxes2Pos.put( vAxis, new Integer( domPos ) );

        domPos++;
      }
      else
      {
        plot.setRangeAxis( ranPos, vAxis );
        plot.setRangeAxisLocation( ranPos, loc );

        chartAxes2Pos.put( vAxis, new Integer( ranPos ) );

        ranPos++;
      }

      diag2chartAxis.put( diagAxes[i], vAxis );
    }

    try
    {
      final IDiagramCurve[] curves = template.getCurveList();
      for( int i = 0; i < curves.length; i++ )
        plot.addCurve( curves[i] );
    }
    catch( SensorException e )
    {
      throw new FactoryException( e );
    }

    return plot;
  }

  /**
   * @param diagAxis
   * @return location according to axis
   */
  private static AxisLocation getLocation( final IDiagramAxis diagAxis )
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
    else if( diagAxis.getPosition().equals( IDiagramAxis.POSITION_LEFT ) )
    {
      if( diagAxis.getPosition().equals( IDiagramAxis.POSITION_BOTTOM ) )
        return AxisLocation.BOTTOM_OR_LEFT;
      else if( diagAxis.getPosition().equals( IDiagramAxis.POSITION_TOP ) )
        return AxisLocation.TOP_OR_LEFT;
    }
    else if( diagAxis.getPosition().equals( IDiagramAxis.POSITION_RIGHT ) )
    {
      if( diagAxis.getPosition().equals( IDiagramAxis.POSITION_BOTTOM ) )
        return AxisLocation.BOTTOM_OR_RIGHT;
      else if( diagAxis.getPosition().equals( IDiagramAxis.POSITION_TOP ) )
        return AxisLocation.TOP_OR_RIGHT;
    }

    // default
    return AxisLocation.BOTTOM_OR_LEFT;
  }
}
package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.Plot;
import org.jfree.chart.renderer.XYItemRenderer;
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
  private final static ConfigurableCachableObjectFactory OF;

  static
  {
    final Properties props = new Properties();
    try
    {
      props.load( ChartFactory.class
          .getResourceAsStream( "resource/type2valueAxis.properties" ) );
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    OF = new ConfigurableCachableObjectFactory( props, false,
        ChartFactory.class.getClassLoader() );
  }

  /**
   * Creates and returns an observation chart.
   * 
   * @param template
   * @return plot
   * 
   * @throws FactoryException
   */
  static Plot createObservationPlot( final IDiagramTemplate template )
      throws FactoryException
  {
    synchronized( template )
    {
      final List diagAxes = template.getDiagramAxes();

      final Map diag2chartAxis = new HashMap( diagAxes.size() );
      final Map chartAxes2Pos = new HashMap( diagAxes.size() );

      final ObservationPlot plot = new ObservationPlot( diag2chartAxis,
          chartAxes2Pos );

      int domPos = 0;
      int ranPos = 0;

      for( final Iterator it = diagAxes.iterator(); it.hasNext(); )
      {
        final IDiagramAxis diagAxis = (IDiagramAxis) it.next();

        ValueAxis vAxis = (ValueAxis) OF.getObjectInstance( diagAxis
            .getDataType(), ValueAxis.class, new Object[] { diagAxis
            .toFullString() } );

        vAxis.setInverted( diagAxis.isInverted() );
        vAxis.setLowerMargin( 0.02 );
        vAxis.setUpperMargin( 0.02 );

        AxisLocation loc = getLocation( diagAxis );

        if( diagAxis.getDirection().equals( IDiagramAxis.DIRECTION_HORIZONTAL ) )
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

        diag2chartAxis.put( diagAxis, vAxis );
      }

      try
      {
        final List curves = template.getCurves();
        for( Iterator it = curves.iterator(); it.hasNext(); )
          plot.addCurve( (IDiagramCurve) it.next() );
      }
      catch( SensorException e )
      {
        throw new FactoryException( e );
      }

      return plot;
    }
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
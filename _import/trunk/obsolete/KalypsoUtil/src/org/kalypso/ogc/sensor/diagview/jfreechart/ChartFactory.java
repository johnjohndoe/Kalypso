package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.io.IOException;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;

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
    Map chartAxes2Pos = new Hashtable( diagAxes.length );
    int domPos = 0;
    int ranPos = 0;

    for( int i = 0; i < diagAxes.length; i++ )
    {
      ValueAxis vAxis = (ValueAxis)m_objFactory.getObjectInstance( diagAxes[i].getDataType(),
          ValueAxis.class, new Object[]
          { diagAxes[i].getLabel() + " [" + diagAxes[i].getUnit() + "]" } );

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

    ICurve[] curves = template.getCurveList();
    for( int i = 0; i < curves.length; i++ )
    {
      try
      {
        CurveDataset cds = new CurveDataset( curves[i] );

        plot.setDataset( i, cds );

        plot.mapDatasetToDomainAxis( i, ( (Integer)chartAxes2Pos.get( diag2chartAxis.get( cds
            .getXDiagAxis() ) ) ).intValue() );
        plot.mapDatasetToRangeAxis( i, ( (Integer)chartAxes2Pos.get( diag2chartAxis.get( cds
            .getYDiagAxis() ) ) ).intValue() );
      }
      catch( SensorException e )
      {
        throw new FactoryException( e );
      }
    }

//    plot.configureDomainAxes();
//    plot.configureRangeAxes();

    plot.setRenderer( new StandardXYItemRenderer( StandardXYItemRenderer.LINES ) );
    JFreeChart chart = new JFreeChart( template.getTitle(), JFreeChart.DEFAULT_TITLE_FONT, plot,
        template.isShowLegend() );

    return chart;
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
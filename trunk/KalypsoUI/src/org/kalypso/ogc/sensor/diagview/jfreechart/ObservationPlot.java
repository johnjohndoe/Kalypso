package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.StandardXYItemRenderer;
import org.jfree.chart.renderer.XYBarRenderer;
import org.jfree.chart.renderer.XYItemRenderer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.IAxisMapping;
import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.util.factory.ConfigurableCachableObjectFactory;
import org.kalypso.util.factory.FactoryException;

/**
 * A plot for IObservation.
 * 
 * @author schlienger
 */
public class ObservationPlot extends XYPlot
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

  /** maps the diagram axis (from the template) to the chart axis */
  private transient final Map m_diag2chartAxis;

  /** maps the chart axis to its position in the plot */
  private transient final Map m_chartAxes2Pos;

  /** maps the diagram axes (from the template) to a dataset */
  private transient final Map m_axes2ds = new HashMap();

  /** default line renderer */
  private static final XYItemRenderer LINE_RENDERER = new StandardXYItemRenderer(
      StandardXYItemRenderer.LINES );

  /** default bar renderer */
  private static final XYItemRenderer BAR_RENDERER = new XYBarRenderer();

  private int m_domPos = 0;

  private int m_ranPos = 0;

  /**
   * Constructor.
   * 
   * @param template
   * @throws SensorException
   */
  public ObservationPlot( final IDiagramTemplate template )
      throws SensorException
  {
    super();

    // standard renderer
    setRenderer( LINE_RENDERER );

    final Collection diagAxes = template.getDiagramAxes();

    m_diag2chartAxis = new HashMap( diagAxes.size() );
    m_chartAxes2Pos = new HashMap( diagAxes.size() );

    for( final Iterator it = diagAxes.iterator(); it.hasNext(); )
    {
      final IDiagramAxis diagAxis = (IDiagramAxis) it.next();

      addDiagramAxis( diagAxis );
    }

    final Collection curves = template.getCurves();
    for( Iterator it = curves.iterator(); it.hasNext(); )
    {
      final IDiagramCurve diagramCurve = (IDiagramCurve) it.next();
      addCurve( diagramCurve );
    }
  }

  /**
   * Adds a diagram axis and configures it for the use in this plot.
   * 
   * @param diagAxis
   * @throws SensorException
   */
  private final void addDiagramAxis( IDiagramAxis diagAxis )
      throws SensorException
  {
    final ValueAxis vAxis;

    try
    {
      vAxis = (ValueAxis) OF.getObjectInstance( diagAxis.getDataType(),
          ValueAxis.class, new Object[] { diagAxis.toFullString() } );
    }
    catch( FactoryException e )
    {
      throw new SensorException( e );
    }

    vAxis.setInverted( diagAxis.isInverted() );
    vAxis.setLowerMargin( 0.02 );
    vAxis.setUpperMargin( 0.02 );

    AxisLocation loc = getLocation( diagAxis );

    if( diagAxis.getDirection().equals( IDiagramAxis.DIRECTION_HORIZONTAL ) )
    {
      setDomainAxis( m_domPos, vAxis );
      setDomainAxisLocation( m_domPos, loc );

      m_chartAxes2Pos.put( vAxis, new Integer( m_domPos ) );

      m_domPos++;
    }
    else
    {
      setRangeAxis( m_ranPos, vAxis );
      setRangeAxisLocation( m_ranPos, loc );

      m_chartAxes2Pos.put( vAxis, new Integer( m_ranPos ) );

      m_ranPos++;
    }

    m_diag2chartAxis.put( diagAxis, vAxis );
  }

  /**
   * Removes all curves from plot.
   */
  public synchronized void clearCurves( )
  {
    for( int i = 0; i < getDatasetCount(); i++ )
      setDataset( i, null );

    m_axes2ds.clear();

    m_chartAxes2Pos.clear();
    m_diag2chartAxis.clear();
    m_domPos = 0;
    m_ranPos = 0;
  }

  /**
   * Adds a curve to the plot
   * 
   * @param curve
   * @throws SensorException
   */
  public synchronized void addCurve( final IDiagramCurve curve )
      throws SensorException
  {
    IAxisMapping[] mings = curve.getMappings();
    IAxis xAxis = null;
    IDiagramAxis xDiagAxis = null;
    IAxis yAxis = null;
    IDiagramAxis yDiagAxis = null;

    for( int i = 0; i < mings.length; i++ )
    {
      final IDiagramAxis diagAxis = mings[i].getDiagramAxis();

      // check if this axis is already present in this plot
      if( !m_diag2chartAxis.containsKey( diagAxis ) )
        addDiagramAxis( diagAxis );

      if( diagAxis.getDirection().equals( IDiagramAxis.DIRECTION_HORIZONTAL ) )
      {
        xAxis = mings[i].getObservationAxis();
        xDiagAxis = diagAxis;
      }
      else
      {
        yAxis = mings[i].getObservationAxis();
        yDiagAxis = diagAxis;
      }
    }

    if( xAxis == null || yAxis == null || xDiagAxis == null
        || yDiagAxis == null )
      throw new IllegalArgumentException( "Kann Kurve " + curve
          + " im Diagramm nicht hinizufügen. Die Achsen sind nicht gültig." );

    final XYCurveSerie xyc = new XYCurveSerie( curve, xAxis, yAxis, xDiagAxis,
        yDiagAxis );

    final String key = xDiagAxis.getIdentifier() + "#-#"
        + yDiagAxis.getIdentifier();

    CurveDataset cds = (CurveDataset) m_axes2ds.get( key );

    if( cds == null )
    {
      cds = new CurveDataset();

      m_axes2ds.put( key, cds );

      int pos = m_axes2ds.values().size();

      setDataset( pos, cds );
      setRenderer( pos, getRenderer( yAxis.getType() ) );

      mapDatasetToDomainAxis( pos, ((Integer) m_chartAxes2Pos
          .get( m_diag2chartAxis.get( xDiagAxis ) )).intValue() );
      mapDatasetToRangeAxis( pos, ((Integer) m_chartAxes2Pos
          .get( m_diag2chartAxis.get( yDiagAxis ) )).intValue() );
    }

    cds.addCurveSerie( xyc );
  }

  /**
   * overriden to return a default axis when no real axes defined yet
   * 
   * @see org.jfree.chart.plot.XYPlot#getDomainAxis()
   */
  public ValueAxis getDomainAxis( )
  {
    synchronized( m_diag2chartAxis )
    {
      if( m_diag2chartAxis.size() == 0 )
        return new NumberAxis();

      return super.getDomainAxis();
    }
  }

  /**
   * overriden to return a default axis when no real axes defined yet
   * 
   * @see org.jfree.chart.plot.XYPlot#getRangeAxis()
   */
  public ValueAxis getRangeAxis( )
  {
    synchronized( m_diag2chartAxis )
    {
      if( m_diag2chartAxis.size() == 0 )
        return new NumberAxis();

      return super.getRangeAxis();
    }
  }
  
  /**
   * Returns the adequate renderer for the given axis type.
   * 
   * @param axisType
   * @return renderer
   */
  private final static XYItemRenderer getRenderer( final String axisType )
  {
    if( axisType.equals( TimeserieConstants.TYPE_RAINFALL ) )
      return BAR_RENDERER;

    return LINE_RENDERER;
  }

  /**
   * @param diagAxis
   * @return location according to axis
   */
  private final static AxisLocation getLocation( final IDiagramAxis diagAxis )
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
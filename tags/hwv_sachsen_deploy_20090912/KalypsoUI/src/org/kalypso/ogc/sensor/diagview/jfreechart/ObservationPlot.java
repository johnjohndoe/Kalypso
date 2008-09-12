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
package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Stroke;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.TimeZone;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.ImageIcon;

import org.jfree.chart.annotations.XYPointerAnnotation;
import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.axis.DateTickUnit;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.TickUnitSource;
import org.jfree.chart.axis.TickUnits;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.IntervalMarker;
import org.jfree.chart.plot.Marker;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.StandardXYItemRenderer;
import org.jfree.chart.renderer.xy.XYBarRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.ui.Align;
import org.jfree.ui.Layer;
import org.jfree.ui.RectangleAnchor;
import org.jfree.ui.RectangleEdge;
import org.jfree.ui.Spacer;
import org.jfree.ui.TextAnchor;
import org.kalypso.auth.KalypsoAuthPlugin;
import org.kalypso.auth.scenario.IScenario;
import org.kalypso.auth.scenario.ScenarioUtilities;
import org.kalypso.commons.factory.ConfigurableCachableObjectFactory;
import org.kalypso.commons.factory.FactoryException;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.AxisMapping;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.DiagViewCurve;
import org.kalypso.ogc.sensor.diagview.DiagramAxis;
import org.kalypso.ogc.sensor.diagview.DiagViewCurve.AlarmLevel;
import org.kalypso.ogc.sensor.template.ObsViewItem;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;

/**
 * A plot for IObservation.
 * 
 * @author schlienger
 */
public class ObservationPlot extends XYPlot
{
  private static final ConfigurableCachableObjectFactory OF;

  static
  {
    final Properties props = new Properties();
    try
    {
      props.load( ChartFactory.class.getResourceAsStream( "resource/type2valueAxis.properties" ) ); //$NON-NLS-1$
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }

    OF = new ConfigurableCachableObjectFactory( props, false, ChartFactory.class.getClassLoader() );
  }

  /** maps the diagram axis (from the template) to the chart axis */
  private transient final Map<DiagramAxis, ValueAxis> m_diag2chartAxis = new HashMap<DiagramAxis, ValueAxis>();

  /** maps the chart axis to its position in the plot */
  private transient final Map<ValueAxis, Integer> m_chartAxes2Pos = new HashMap<ValueAxis, Integer>();

  /** maps the diagram axes (from the template) to a dataset */
  private transient final Map<DiagramAxis, CurveDataset> m_diagAxis2ds = new HashMap<DiagramAxis, CurveDataset>();

  /** maps the diagram curve to the data serie */
  private transient final Map<DiagViewCurve, XYCurveSerie> m_curve2serie = new HashMap<DiagViewCurve, XYCurveSerie>();

  /** maps the series to their datasets */
  private transient final Map<XYCurveSerie, CurveDataset> m_serie2dataset = new HashMap<XYCurveSerie, CurveDataset>();

  private transient final Map<Double, AlarmLevelPlotElement> m_yConsts = new HashMap<Double, AlarmLevelPlotElement>();

  private transient final Map<Long, Marker> m_markers = new HashMap<Long, Marker>();

  /** is true as soon as one background image has been set */
  private boolean m_bgImageSet = false;

  private TimeZone m_timezone;

  /**
   * Constructor.
   */
  public ObservationPlot( final DiagView view ) throws SensorException
  {
    // space between axes and data area
    setAxisOffset( new Spacer( Spacer.ABSOLUTE, 5, 5, 5, 5 ) );

    // standard renderer
    setRenderer( new StandardXYItemRenderer( StandardXYItemRenderer.LINES ) );

    final DiagramAxis[] diagAxes = view.getDiagramAxes();
    for( final DiagramAxis diagAxis : diagAxes )
    {
      addDiagramAxis( diagAxis, null );
    }

    final ObsViewItem[] curves = view.getItems();
    for( final ObsViewItem element : curves )
      addCurve( (DiagViewCurve) element );

    setNoDataMessage( Messages.getString("org.kalypso.ogc.sensor.diagview.jfreechart.ObservationPlot.1") ); //$NON-NLS-1$

    setTimezone( view.getTimezone() );
  }

  public void dispose( )
  {
    clearCurves();
  }

  /**
   * Adds a diagram axis and configures it for the use in this plot.
   * 
   * @param axis
   *          can be null, if present it is used to define a best suited formater for the chart axis
   */
  private synchronized final void addDiagramAxis( final DiagramAxis diagAxis, final IAxis axis ) throws SensorException
  {
    if( diagAxis == null )
      throw new IllegalArgumentException( "DiagramAxis is null" );

    ValueAxis vAxis;

    try
    {
      final String dataType = diagAxis.getDataType();
      final TimeZone timezone = KalypsoCorePlugin.getDefault().getTimeZone();

      if( "date".equals( dataType ) )
      {
        // HACK: instantiation is not possible via factroy, as the search for the constructor is buggy there...
        vAxis = new DateAxis( diagAxis.toFullString(), timezone );
        final DateAxis da = (DateAxis)vAxis;

        //      REMARK: the next line is necessary, as the constructor with timezone does
        // not initalize the timeline (freechart bug!)
        da.setTimeline( new DefaultTimeline() );
        // Create standard source with correct timezone
        final TickUnitSource source = createStandardDateTickUnits( timezone );
        da.setStandardTickUnits( source );
        //        da.setDateFormatOverride( TimeserieUtils.getDateFormat() );
      }
      else
        vAxis = (ValueAxis)OF.getObjectInstance( dataType, ValueAxis.class, new Object[]
        { diagAxis.toFullString() } );

      // HACK: damit immer zu mindest [0,1] als range gesetzt wird
      // z.Zt. nur für Niederschlag.
      if( vAxis instanceof NumberAxis && TimeserieConstants.TYPE_RAINFALL.equals( axis == null ? null : axis.getType() ) )
      {
        final NumberAxis na = (NumberAxis)vAxis;
        na.setAutoRangeMinimumSize( 1 );

        if( na instanceof NumberAxis2 )
        {
          final NumberAxis2 na2 = (NumberAxis2)na;
          na2.setMin( new Double( 0 ) );
          na2.setMax( new Double( 1 ) );
        }
      }    }
    catch( final FactoryException e )
    {
      throw new SensorException( e );
    }

    // TODO: if date axis: set timezone
    setTimezone( vAxis );

    vAxis.setInverted( diagAxis.isInverted() );

    if( diagAxis.getLowerMargin() != null )
      vAxis.setLowerMargin( diagAxis.getLowerMargin().doubleValue() );

    if( diagAxis.getUpperMaring() != null )
      vAxis.setUpperMargin( diagAxis.getUpperMaring().doubleValue() );

    final AxisLocation loc = getLocation( diagAxis );

    if( diagAxis.getDirection().equals( DiagramAxis.DIRECTION_HORIZONTAL ) )
    {
      final int pos = getAdequateDomainPos();
      setDomainAxis( pos, vAxis );
      setDomainAxisLocation( pos, loc );

      m_chartAxes2Pos.put( vAxis, new Integer( pos ) );
    }
    else
    {
      final int pos = getAdequateRangePos();
      setRangeAxis( pos, vAxis );
      setRangeAxisLocation( pos, loc );

      m_chartAxes2Pos.put( vAxis, new Integer( pos ) );
    }

    m_diag2chartAxis.put( diagAxis, vAxis );
  }

  /**
   * @return adequate position for a new domain axis
   */
  private int getAdequateDomainPos( )
  {
    final int count = getDomainAxisCount();
    if( count == 0 )
      return 0;

    for( int i = 0; i < count; i++ )
      if( getDomainAxis( i ) == null )
        return i;

    return count;
  }

  /**
   * @return adequate position for a new range axis
   */
  private int getAdequateRangePos( )
  {
    final int count = getRangeAxisCount();
    if( count == 0 )
      return 0;

    for( int i = 0; i < count; i++ )
      if( getRangeAxis( i ) == null )
        return i;

    return count;
  }

  /**
   * @return adequate position for a new dataset
   */
  private int getAdequateDatasetPos( )
  {
    final int count = getDatasetCount();
    if( count == 0 )
      return 0;

    for( int i = 0; i < count; i++ )
      if( getDataset( i ) == null )
        return i;

    return count;
  }

  /**
   * Removes all curves from plot.
   */
  public synchronized void clearCurves( )
  {
    for( int i = 0; i < getDatasetCount(); i++ )
      setDataset( i, null );

    clearBackground();

    m_serie2dataset.clear();
    m_curve2serie.clear();

    m_diagAxis2ds.clear();

    m_chartAxes2Pos.clear();
    m_diag2chartAxis.clear();

    clearDomainMarkers();
    clearAnnotations();

    m_yConsts.clear();
    m_markers.clear();

    clearDomainAxes();
    clearRangeAxes();
  }

  /**
   * Adds a curve to the plot
   */
  public synchronized final void addCurve( final DiagViewCurve curve ) throws SensorException
  {
    if( curve == null || !curve.isShown() || m_curve2serie.containsKey( curve ) )
      return;

    final AxisMapping[] mings = curve.getMappings();

    IAxis xAxis = null;
    DiagramAxis xDiagAxis = null;
    IAxis yAxis = null;
    DiagramAxis yDiagAxis = null;

    if( mings != null )
    {
      for( final AxisMapping element : mings )
      {
        final DiagramAxis diagAxis = element.getDiagramAxis();

        if( diagAxis == null )
          continue;

        // check if this axis is already present in this plot
        if( !m_diag2chartAxis.containsKey( diagAxis ) )
          addDiagramAxis( diagAxis, element.getObservationAxis() );

        if( diagAxis.getDirection().equals( DiagramAxis.DIRECTION_HORIZONTAL ) )
        {
          xAxis = element.getObservationAxis();
          xDiagAxis = diagAxis;
        }
        else
        {
          yAxis = element.getObservationAxis();
          yDiagAxis = diagAxis;
        }
      }
    }

    if( xAxis == null || yAxis == null || xDiagAxis == null || yDiagAxis == null )
      throw new IllegalArgumentException( Messages.getString("org.kalypso.ogc.sensor.diagview.jfreechart.ObservationPlot.2") + curve + Messages.getString("org.kalypso.ogc.sensor.diagview.jfreechart.ObservationPlot.3") ); //$NON-NLS-1$ //$NON-NLS-2$

    final XYCurveSerie serie = new XYCurveSerie( curve, xAxis, yAxis, xDiagAxis, yDiagAxis );

    m_curve2serie.put( curve, serie );

    final DiagramAxis key = yDiagAxis;

    CurveDataset cds = m_diagAxis2ds.get( key );

    if( cds == null )
    {
      cds = new CurveDataset();

      m_diagAxis2ds.put( key, cds );

      final int pos = getAdequateDatasetPos();
      setDataset( pos, cds );

      final XYItemRenderer renderer = getRenderer( yAxis.getType() );
      setRenderer( pos, renderer );

      mapDatasetToDomainAxis( pos, m_chartAxes2Pos.get( m_diag2chartAxis.get( xDiagAxis ) ).intValue() );
      mapDatasetToRangeAxis( pos, m_chartAxes2Pos.get( m_diag2chartAxis.get( yDiagAxis ) ).intValue() );
    }

    // UGLY TRICK: if it's a rainfall axis, set the range of the diagram axis to [0 - 1]
    if( yAxis.getType().equals( TimeserieConstants.TYPE_RAINFALL ) )
    {
      final ValueAxis nAxis = m_diag2chartAxis.get( yDiagAxis );
      nAxis.setAutoRangeMinimumSize( 1 );
    }

    // if a curve gets removed meanwhile, the mapping seriespos -> curvecolor
    // gets invalid! always reset all colors of all curves
    final Color curveColor = curve.getColor();
    final Stroke curveStroke = curve.getStroke();
    cds.addCurveSerie( serie, curveColor, curveStroke, getRenderer( indexOf( cds ) ) );

    m_serie2dataset.put( serie, cds );

    analyseCurve( curve );
  }

  private void analyseCurve( final DiagViewCurve curve ) throws SensorException
  {
    final IObservation obs = curve.getObservation();

    if( curve.getView().isFeatureEnabled( TimeserieConstants.FEATURE_FORECAST ) )
    {
      // add a marker if the obs is a forecast
      final DateRange fr = TimeserieUtils.isForecast( obs );
      if( fr != null )
      {
        final Long begin = new Long( fr.getFrom().getTime() );
        if( !m_markers.containsKey( begin ) )
        {
          final long end = fr.getTo().getTime();
          final Marker marker = createMarker( begin.doubleValue(), end, TimeserieConstants.MD_VORHERSAGE, TimeserieUtils.getColorForMD( TimeserieConstants.MD_VORHERSAGE ) );

          addDomainMarker( marker, Layer.BACKGROUND );

          m_markers.put( begin, marker );
        }
      }
    }

    if( obs != null )
    {
      final MetadataList mdl = obs.getMetadataList();

      // TODO: move background image stuff into template

      // change diagram background if obs has scenario specific metadata property
      if( mdl.getProperty( ObservationConstants.MD_SCENARIO ) != null )
      {
        final IScenario scenario = KalypsoAuthPlugin.getDefault().getScenario( mdl.getProperty( ObservationConstants.MD_SCENARIO ) );

        if( scenario != null && !ScenarioUtilities.isDefaultScenario( scenario ) && !m_bgImageSet )
        {
          final String imageURL = scenario.getProperty( IScenario.PROP_DIAG_BACKGROUND_IMAGE_URL, null );
          if( imageURL != null )
          {
            try
            {
              final Image image = new ImageIcon( new URL( imageURL ) ).getImage();
              setBackgroundImage( image );
              setBackgroundImageAlignment( Align.FIT_HORIZONTAL | Align.NORTH );
              m_bgImageSet = true;
            }
            catch( final MalformedURLException e )
            {
              Logger.getLogger( getClass().getName() ).log( Level.WARNING, Messages.getString("org.kalypso.ogc.sensor.diagview.jfreechart.ObservationPlot.4"), e ); //$NON-NLS-1$
            }
          }
        }
      }

      // add a constant Y line if obs has alarmstufen
      if( curve.isDisplayAlarmLevel() )
      {
        final AlarmLevel[] alarms = curve.getAlarmLevels();
        for( final AlarmLevel element : alarms )
        {
          final Double value = new Double( element.value );
          if( !m_yConsts.containsKey( value ) )
          {
            final XYCurveSerie xyc = m_curve2serie.get( curve );
            final double x;
            if( xyc.getItemCount() > 1 )
              x = xyc.getXValue( 1 ).doubleValue();
            else
              x = getDomainAxis().getLowerBound();

            final AlarmLevelPlotElement vac = new AlarmLevelPlotElement( element, x, xyc.getYDiagAxis() );
            m_yConsts.put( value, vac );
          }
        }
      }
    }
  }

  /**
   * Refreshes the plot in order to take the enabled features of the view into account
   */
  public void refreshMetaInformation( )
  {
    // clear all markers and extra informations
    clearDomainMarkers();
    clearAnnotations();
    m_yConsts.clear();
    m_markers.clear();

    // step through curves and analyse them

    for( final Object element : m_curve2serie.keySet() )
    {
      final DiagViewCurve curve = (DiagViewCurve) element;

      try
      {
        analyseCurve( curve );
      }
      catch( final SensorException e )
      {
        Logger.getLogger( getClass().getName() ).warning( e.getLocalizedMessage() );
      }
    }
  }

  /**
   * Removes the curve from the plot
   */
  public synchronized void removeCurve( final DiagViewCurve curve )
  {
    final XYCurveSerie serie = m_curve2serie.get( curve );

    if( serie != null )
    {
      final CurveDataset ds = m_serie2dataset.get( serie );

      if( ds != null )
      {
        ds.removeCurveSerie( serie );

        // if dataset is empty, also remove it and the range axis to which it
        // belongs
        if( ds.getSeriesCount() == 0 )
        {
          // and remove the dataset
          for( int i = 0; i < getDatasetCount(); i++ )
          {
            if( getDataset( i ) == ds )
            {
              setDataset( i, null );

              break;
            }
          }

          // step though axes and remove the one that is associated to
          // the dataset we want to remove
          final Iterator<DiagramAxis> it = m_diagAxis2ds.keySet().iterator();
          while( it.hasNext() )
          {
            final DiagramAxis dAxis = it.next();
            if( m_diagAxis2ds.get( dAxis ) == ds )
            {
              final ValueAxis cAxis = m_diag2chartAxis.get( dAxis );
              final Integer pos = m_chartAxes2Pos.get( cAxis );

              // trick: if it is the only axis, then do not remove it
              // else NullPointerException in drawQuadrants (JFreeChart)
              if( getRangeAxis() != getRangeAxis( pos.intValue() ) || getRangeAxisCount() > 1 )
              {
                setRangeAxis( pos.intValue(), null );
                // m_chartAxes2Pos.remove( cAxis );
                m_diag2chartAxis.remove( dAxis );
              }

              it.remove();

              // break, that's it
              break;
            }
          }
        }
      }

      m_curve2serie.remove( curve );

      if( m_curve2serie.size() == 0 )
        clearCurves();
    }
  }

  private void clearBackground( )
  {
    setBackgroundImage( null );
    m_bgImageSet = false;
  }

  /**
   * overriden to return a default axis when no real axes defined yet
   * 
   * @see org.jfree.chart.plot.XYPlot#getDomainAxis()
   */
  @Override
  public synchronized ValueAxis getDomainAxis( )
  {
    if( m_diag2chartAxis.size() == 0 )
      return new NumberAxis();

    return super.getDomainAxis();
  }

  /**
   * Overriden to return a default axis when no real axes defined yet
   * 
   * @see org.jfree.chart.plot.XYPlot#getRangeAxis()
   */
  @Override
  public synchronized ValueAxis getRangeAxis( )
  {
    if( m_diag2chartAxis.size() == 0 )
      return new NumberAxis();

    for( int i = 0; i < getRangeAxisCount(); i++ )
    {
      final ValueAxis rangeAxis = getRangeAxis( i );
      if( rangeAxis != null )
        return rangeAxis;
    }

    return new NumberAxis();
  }

  /**
   * overriden to also draw our alarmlevels
   * 
   * @see org.jfree.chart.plot.XYPlot#drawAnnotations(java.awt.Graphics2D, java.awt.geom.Rectangle2D,
   *      org.jfree.chart.plot.PlotRenderingInfo)
   */
  @Override
  public synchronized void drawAnnotations( final Graphics2D g2d, final Rectangle2D rec, final PlotRenderingInfo arg2 )
  {
    super.drawAnnotations( g2d, rec, arg2 );

    drawAlarmLevels( g2d, rec );
  }

  /**
   * Draw alarmlevels (horizontal line and text annotation)
   */
  private void drawAlarmLevels( final Graphics2D g2, final Rectangle2D dataArea )
  {
    for( final Object element : m_yConsts.keySet() )
    {
      final AlarmLevelPlotElement vac = m_yConsts.get( element );

      final ValueAxis axis = m_diag2chartAxis.get( vac.axis );
      if( axis == null )
        continue;

      if( axis.getRange().contains( vac.alarm.value ) )
      {
        final double yy = axis.valueToJava2D( vac.alarm.value, dataArea, RectangleEdge.LEFT );
        final Line2D line = new Line2D.Double( dataArea.getMinX(), yy, dataArea.getMaxX(), yy );
        // always set stroke, else we got the stroke from the last drawn line
        g2.setStroke( AlarmLevelPlotElement.STROKE_ALARM ); 
        g2.setPaint( vac.alarm.color );
        g2.draw( line );

        // and draw the text annotation
        vac.annotation.draw( g2, this, dataArea, getDomainAxis(), axis );
      }
    }
  }

  /**
   * Helper that creates a marker
   */
  private final static Marker createMarker( final double start, final double end, final String label, final Color color )
  {
    final IntervalMarker marker = new IntervalMarker( start, end );
    marker.setPaint( color );
    marker.setLabel( label );
    marker.setLabelAnchor( RectangleAnchor.CENTER );
    marker.setLabelTextAnchor( TextAnchor.CENTER );

    return marker;
  }

  /**
   * Returns the adequate renderer for the given axis type.
   */
  private final XYItemRenderer getRenderer( final String axisType )
  {
    if( axisType.equals( TimeserieConstants.TYPE_RAINFALL ) )
      return new XYBarRenderer();

    if( axisType.equals( TimeserieConstants.TYPE_POLDER_CONTROL ) )
      return new XYBarRenderer();
    
    return new StandardXYItemRenderer( StandardXYItemRenderer.LINES );
  }

  /**
   * @param diagAxis
   * @return location according to axis
   */
  private final static AxisLocation getLocation( final DiagramAxis diagAxis )
  {
    if( diagAxis.getPosition().equals( DiagramAxis.POSITION_BOTTOM ) )
    {
      return AxisLocation.BOTTOM_OR_LEFT;
    }
    else if( diagAxis.getPosition().equals( DiagramAxis.POSITION_TOP ) )
    {
      return AxisLocation.TOP_OR_LEFT;
    }
    else if( diagAxis.getPosition().equals( DiagramAxis.POSITION_LEFT ) )
    {
      return AxisLocation.TOP_OR_LEFT;
    }
    else if( diagAxis.getPosition().equals( DiagramAxis.POSITION_RIGHT ) )
    {
      return AxisLocation.TOP_OR_RIGHT;
    }

    // default
    return AxisLocation.BOTTOM_OR_LEFT;
  }

  /**
   * Special tick units for kalypso
   */
  public static TickUnitSource createStandardDateTickUnits( TimeZone zone )
  {

    if( zone == null )
    {
      throw new IllegalArgumentException( "Null 'zone' argument." );
    }
    TickUnits units = new TickUnits();

    // date formatters
    //      DateFormat f1 = new SimpleDateFormat("HH:mm:ss.SSS");
    //      DateFormat f2 = new SimpleDateFormat("HH:mm:ss");
    //      DateFormat f3 = new SimpleDateFormat("HH:mm");
    //      DateFormat f4 = new SimpleDateFormat("d-MMM, HH:mm");
    //      DateFormat f5 = new SimpleDateFormat("d-MMM");
    //      DateFormat f6 = new SimpleDateFormat("MMM-yyyy");
    //      DateFormat f7 = new SimpleDateFormat("yyyy");

    DateFormat f1 = new SimpleDateFormat( "dd.MM HH:mm:ss.SSS" );
    DateFormat f2 = new SimpleDateFormat( "dd.MM HH:mm:ss" );
    DateFormat f3 = new SimpleDateFormat( "dd.MM HH:mm" );
    DateFormat f4 = new SimpleDateFormat( "dd.MM HH:mm" );
    DateFormat f5 = new SimpleDateFormat( "dd.MM" );
    DateFormat f6 = new SimpleDateFormat( "dd.MM.yy" );
    DateFormat f7 = new SimpleDateFormat( "yyyy" );

    f1.setTimeZone( zone );
    f2.setTimeZone( zone );
    f3.setTimeZone( zone );
    f4.setTimeZone( zone );
    f5.setTimeZone( zone );
    f6.setTimeZone( zone );
    f7.setTimeZone( zone );

    // milliseconds
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 1, f1 ) );
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 5, DateTickUnit.MILLISECOND, 1, f1 ) );
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 10, DateTickUnit.MILLISECOND, 1, f1 ) );
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 25, DateTickUnit.MILLISECOND, 5, f1 ) );
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 50, DateTickUnit.MILLISECOND, 10, f1 ) );
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 100, DateTickUnit.MILLISECOND, 10, f1 ) );
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 250, DateTickUnit.MILLISECOND, 10, f1 ) );
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 500, DateTickUnit.MILLISECOND, 50, f1 ) );

    // seconds
    units.add( new DateTickUnit( DateTickUnit.SECOND, 1, DateTickUnit.MILLISECOND, 50, f2 ) );
    units.add( new DateTickUnit( DateTickUnit.SECOND, 5, DateTickUnit.SECOND, 1, f2 ) );
    units.add( new DateTickUnit( DateTickUnit.SECOND, 10, DateTickUnit.SECOND, 1, f2 ) );
    units.add( new DateTickUnit( DateTickUnit.SECOND, 30, DateTickUnit.SECOND, 5, f2 ) );

    // minutes
    units.add( new DateTickUnit( DateTickUnit.MINUTE, 1, DateTickUnit.SECOND, 5, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.MINUTE, 2, DateTickUnit.SECOND, 10, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.MINUTE, 5, DateTickUnit.MINUTE, 1, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.MINUTE, 10, DateTickUnit.MINUTE, 1, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.MINUTE, 15, DateTickUnit.MINUTE, 5, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.MINUTE, 20, DateTickUnit.MINUTE, 5, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.MINUTE, 30, DateTickUnit.MINUTE, 5, f3 ) );

    // hours
    units.add( new DateTickUnit( DateTickUnit.HOUR, 1, DateTickUnit.MINUTE, 5, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.HOUR, 2, DateTickUnit.MINUTE, 10, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.HOUR, 4, DateTickUnit.MINUTE, 30, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.HOUR, 6, DateTickUnit.HOUR, 1, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.HOUR, 12, DateTickUnit.HOUR, 1, f4 ) );

    // days
    units.add( new DateTickUnit( DateTickUnit.DAY, 1, DateTickUnit.HOUR, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 2, DateTickUnit.HOUR, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 3, DateTickUnit.HOUR, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 4, DateTickUnit.HOUR, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 5, DateTickUnit.HOUR, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 6, DateTickUnit.HOUR, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 7, DateTickUnit.DAY, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 10, DateTickUnit.DAY, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 15, DateTickUnit.DAY, 1, f5 ) );

    // months
    units.add( new DateTickUnit( DateTickUnit.MONTH, 1, DateTickUnit.DAY, 1, f6 ) );
    units.add( new DateTickUnit( DateTickUnit.MONTH, 2, DateTickUnit.DAY, 1, f6 ) );
    units.add( new DateTickUnit( DateTickUnit.MONTH, 3, DateTickUnit.MONTH, 1, f6 ) );
    units.add( new DateTickUnit( DateTickUnit.MONTH, 4, DateTickUnit.MONTH, 1, f6 ) );
    units.add( new DateTickUnit( DateTickUnit.MONTH, 6, DateTickUnit.MONTH, 1, f6 ) );

    // years
    units.add( new DateTickUnit( DateTickUnit.YEAR, 1, DateTickUnit.MONTH, 1, f7 ) );
    units.add( new DateTickUnit( DateTickUnit.YEAR, 2, DateTickUnit.MONTH, 3, f7 ) );
    units.add( new DateTickUnit( DateTickUnit.YEAR, 5, DateTickUnit.YEAR, 1, f7 ) );
    units.add( new DateTickUnit( DateTickUnit.YEAR, 10, DateTickUnit.YEAR, 1, f7 ) );
    units.add( new DateTickUnit( DateTickUnit.YEAR, 25, DateTickUnit.YEAR, 5, f7 ) );
    units.add( new DateTickUnit( DateTickUnit.YEAR, 50, DateTickUnit.YEAR, 10, f7 ) );
    units.add( new DateTickUnit( DateTickUnit.YEAR, 100, DateTickUnit.YEAR, 20, f7 ) );

    return units;

  }

  /**
   * mini helper class for storing a value and a color
   * 
   * @author schlienger
   */
  private final static class AlarmLevelPlotElement
  {
    /** Stroke, with wich the alarm-levels get drawn */
    public static final Stroke STROKE_ALARM = new BasicStroke( 1.0f );

    final AlarmLevel alarm;

    final String label;

    final DiagramAxis axis;

    final XYPointerAnnotation annotation;

    public AlarmLevelPlotElement( final AlarmLevel al, final double xCoord, final DiagramAxis diagAxis )
    {
      this.alarm = al;
      this.label = al.label + " (" + al.value + ")"; //$NON-NLS-1$ //$NON-NLS-2$
      this.axis = diagAxis;
      //      this.annotation = new XYTextAnnotation( al.label, xCoord, al.value );
      annotation = new XYPointerAnnotation( al.label, xCoord, al.value, 0 );
      annotation.setAngle( Math.toRadians( 340 ) );
      annotation.setArrowLength( 10.0 );
      annotation.setLabelOffset( 30 );
      //      annotation.setArrowPaint( new Color( 0, 0, 0, 0 ) ); // invisible
      annotation.setArrowPaint( al.color );
      annotation.setPaint( al.color );
    }

    @Override
    public String toString( )
    {
      return getClass().getName() + ": " + this.label + " " + this.alarm + " " + this.axis.getLabel(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
  }

  public void setTimezone( final TimeZone timezone )
  {
    m_timezone = timezone;

    for( int i = 0; i < getDomainAxisCount(); i++ )
    {
      final ValueAxis axis = getDomainAxis( i );
      setTimezone( axis );
    }

    for( int i = 0; i < getRangeAxisCount(); i++ )
    {
      final ValueAxis axis = getRangeAxis( i );
      setTimezone( axis );
    }
  }

  private void setTimezone( final ValueAxis axis )
  {
    if( axis instanceof DateAxis )
    {
      final DateAxis da = (DateAxis) axis;
      final DateFormat df = da.getDateFormatOverride() != null ? da.getDateFormatOverride() : DateFormat.getDateTimeInstance();
      df.setTimeZone( m_timezone );
      da.setDateFormatOverride( df );
    }
  }
}
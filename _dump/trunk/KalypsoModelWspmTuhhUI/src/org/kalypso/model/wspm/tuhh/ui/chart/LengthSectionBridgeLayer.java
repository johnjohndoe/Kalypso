package org.kalypso.model.wspm.tuhh.ui.chart;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.RectangleUtils;

import de.openali.odysseus.chart.ext.base.layer.AbstractLineLayer;
import de.openali.odysseus.chart.framework.model.data.IDataOperator;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.data.impl.DataRange;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;

public class LengthSectionBridgeLayer extends AbstractLineLayer
{

  private LengthSectionBridgeDataContainer< ? , ? > m_data;

  private String m_tooltip = null;

  public void setTooltip( String tooltip )
  {
    m_tooltip = tooltip;
  }

  public LengthSectionBridgeLayer( final LengthSectionBridgeDataContainer< ? , ? > data, final ILineStyle lineStyle, final IPointStyle pointStyle )
  {
    super( lineStyle, pointStyle );
    m_data = data;
  }

  @Override
  public void drawIcon( final Image img )
  {
    final Rectangle bounds = img.getBounds();
    final int height = bounds.height;
    final int width = bounds.width;
    final GC gc = new GC( img );

    final ArrayList<Point> path = new ArrayList<Point>();

    path.add( new Point( 0, height / 2 ) );
    path.add( new Point( width / 5, height / 2 ) );
    path.add( new Point( width / 5 * 2, height / 4 ) );
    path.add( new Point( width / 5 * 3, height / 4 * 3 ) );
    path.add( new Point( width / 5 * 4, height / 2 ) );
    path.add( new Point( width, height / 2 ) );

    drawLine( gc, path );
    gc.dispose();

  }

  @SuppressWarnings("unchecked")
  public void paint( final GC gc )
  {
    if( m_data == null )
      return;

    final List<Point> path = new ArrayList<Point>();

    m_data.open();

    // final TupleResult result = m_data.getTupleResult();

    final Object[] domainValues = m_data.getDomainValues();
    final Object[] targetValues = m_data.getTargetValues();
    final Object[] uKValues = m_data.getBridgeValues();

    if( domainValues.length > 0 && targetValues.length > 0 )
    {
      final IAxis domainAxis = getDomainAxis();
      final IAxis targetAxis = getTargetAxis();
      final IDataOperator dopDomain = domainAxis.getDataOperator( domainAxis.getDataClass() );// domainValues[0].getClass()
      // );
      final IDataOperator dopTarget = targetAxis.getDataOperator( targetAxis.getDataClass() );// targetValues[0].getClass()
      // );

      if( dopDomain == null || dopTarget == null )
        return;

      for( int i = 0; i < domainValues.length; i++ )
      {
        final Number domainValue = dopDomain.logicalToNumeric( domainValues[i] );
        final Number domainValLeft = i > 0 ? dopDomain.logicalToNumeric( domainValues[i - 1] ) : domainValue;
        final Number domainValRight = i < domainValues.length - 1 ? dopDomain.logicalToNumeric( domainValues[i + 1] ) : domainValue;
        final Number targetValue = dopTarget.logicalToNumeric( targetValues[i] );
        final Number uKValue = dopTarget.logicalToNumeric( uKValues[i] );

        // we have to check if all values are correct - an incorrect value means a null value - the axis would return 0
        // in that case
        if( domainValue != null && targetValue != null && uKValue != null )
        {
          final Point OKLeft = getCoordinateMapper().numericToScreen( (Double) domainValLeft + ((Double) domainValue - (Double) domainValLeft) / 2.0, targetValue );
          final Point UKLeft = getCoordinateMapper().numericToScreen( (Double) domainValLeft + ((Double) domainValue - (Double) domainValLeft) / 2.0, uKValue );
          final Point OKRight = getCoordinateMapper().numericToScreen( (Double) domainValue + ((Double) domainValRight - (Double) domainValue) / 2.0, targetValue );
          final Point UKRight = getCoordinateMapper().numericToScreen( (Double) domainValue + ((Double) domainValRight - (Double) domainValue) / 2.0, uKValue );
          path.add( OKLeft );
          path.add( OKRight );
          path.add( UKRight );
          path.add( UKLeft );
          path.add( OKLeft );
        }
      }
    }
    drawLine( gc, path );
  }

 

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#getDomainRange()
   */
  @SuppressWarnings("unchecked")
  public IDataRange getDomainRange( )
  {
    if( m_data == null )
      return null;

    final IDataRange dataRange = m_data.getDomainRange();

    final Object min = dataRange.getMin();
    final Object max = dataRange.getMax();
    if( min == null || max == null )
      return null;

    final IDataOperator dop = getDomainAxis().getDataOperator( min.getClass() );
    final IDataRange numRange = new DataRange<Number>( dop.logicalToNumeric( min ), dop.logicalToNumeric( max ) );
    return numRange;
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#getTargetRange()
   */
  @SuppressWarnings("unchecked")
  public IDataRange<Number> getTargetRange( )
  {
    if( m_data == null )
      return null;

    final IDataRange dataRange = m_data.getTargetRange();
    final Object min = dataRange.getMin();
    final Object max = dataRange.getMax();
    if( min == null || max == null )
      return null;

    final IDataOperator dop = getTargetAxis().getDataOperator( max.getClass() );
    final IDataRange<Number> numRange = new DataRange<Number>( dop.logicalToNumeric( min ), dop.logicalToNumeric( max ) );
    return numRange;
  }
  /**
   * @see de.openali.odysseus.chart.framework.model.layer.ITooltipChartLayer#getHover(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHover( Point pos )
  {
    if( !isVisible() || m_tooltip == null )
      return null;
    final IAxis domainAxis = getDomainAxis();
    final IAxis targetAxis = getTargetAxis();
    final IDataOperator dopDomain = domainAxis.getDataOperator( domainAxis.getDataClass() );
    final IDataOperator dopTarget = targetAxis.getDataOperator( targetAxis.getDataClass() );
    final Object[] domainValues = m_data.getDomainValues();
    final Object[] targetValues = m_data.getTargetValues();

    for( int i = 0; i < domainValues.length; i++ )
    {
      final Object domainValue = domainValues[i];
      final Object targetValue = targetValues[i];
      if( targetValue == null )
        continue;
      final Point pValue = getCoordinateMapper().numericToScreen( dopDomain.logicalToNumeric( domainValue ), dopTarget.logicalToNumeric( targetValue ) );
      final Rectangle hover = RectangleUtils.buffer( pValue );
      if( hover == null )
        continue;

      if( hover.contains( pos ) )
      {
        if( pValue == null )
          return new EditInfo( this, null, null, i, m_tooltip, RectangleUtils.getCenterPoint( hover ) );

        return new EditInfo( this, null, null, i, m_tooltip, pValue );
      }
    }

    return null;
  }
  @SuppressWarnings("unchecked")
  protected void setData( final LengthSectionBridgeDataContainer data )
  {
    m_data = data;
  }
}

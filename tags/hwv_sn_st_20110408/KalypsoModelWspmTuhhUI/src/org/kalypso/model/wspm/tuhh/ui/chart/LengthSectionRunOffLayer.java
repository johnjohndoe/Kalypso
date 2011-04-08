package org.kalypso.model.wspm.tuhh.ui.chart;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.kalypso.chart.ext.observation.data.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.layer.TupleResultLineLayer;

import de.openali.odysseus.chart.framework.model.data.IDataOperator;
import de.openali.odysseus.chart.framework.model.layer.ILayerProvider;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;

public class LengthSectionRunOffLayer extends TupleResultLineLayer
{
  /**
   * @see org.kalypso.chart.ext.observation.layer.TupleResultLineLayer#getTitle()
   */

  public LengthSectionRunOffLayer( final ILayerProvider provider, final TupleResultDomainValueData< ? , ? > data, final ILineStyle lineStyle, final IPointStyle pointStyle )
  {
    super( provider, data, lineStyle, pointStyle );

  }

  @Override
  public void paint( final GC gc )
  {
    if( m_data == null )
      return;

    final List<Point> path = new ArrayList<Point>();

    m_data.open();

    final Object[] domainValues = m_data.getDomainValues();
    final Object[] targetValues = m_data.getTargetValues();

    if( domainValues.length > 0 && targetValues.length > 0 )
    {
      final IAxis domainAxis = getDomainAxis();
      final IAxis targetAxis = getTargetAxis();
      final IDataOperator dopDomain = domainAxis.getDataOperator( domainAxis.getDataClass() );
      final IDataOperator dopTarget = targetAxis.getDataOperator( targetAxis.getDataClass() );

      if( dopDomain == null || dopTarget == null )
        return;

      for( int i = 0; i < domainValues.length; i++ )
      {
        final Object domainValue = domainValues[i];
        final Object targetValue = targetValues[i];

        // we have to check if all values are correct - an incorrect value means a null value - the axis would return 0
        // in that case
        if( domainValue != null && targetValue != null )
        {
          final Point screen = getCoordinateMapper().numericToScreen( dopDomain.logicalToNumeric( domainValue ), dopTarget.logicalToNumeric( targetValue ) );
          path.add( screen );
          if( i < domainValues.length - 1 && ((Number) targetValue).doubleValue() != ((Number) targetValues[i + 1]).doubleValue() )
          {
            path.add( getCoordinateMapper().numericToScreen( dopDomain.logicalToNumeric( domainValues[i + 1] ), dopTarget.logicalToNumeric( targetValue ) ) );
          }
        }
      }
    }

    drawLine( gc, path );
    drawPoints( gc, path );
  }

}

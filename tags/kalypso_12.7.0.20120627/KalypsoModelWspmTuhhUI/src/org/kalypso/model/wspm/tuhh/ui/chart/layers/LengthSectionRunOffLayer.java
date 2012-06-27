package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.kalypso.chart.ext.observation.data.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.layer.TupleResultLineLayer;

import de.openali.odysseus.chart.framework.model.layer.ILayerProvider;
import de.openali.odysseus.chart.framework.model.style.IStyleSet;

public class LengthSectionRunOffLayer extends TupleResultLineLayer
{
  /**
   * @see org.kalypso.chart.ext.observation.layer.TupleResultLineLayer#getTitle()
   */

  public LengthSectionRunOffLayer( final ILayerProvider provider, final TupleResultDomainValueData< ? , ? > data, final IStyleSet styleSet)
  {
    super( provider, data, styleSet );

  }

  @Override
  public void paint( final GC gc )
  {
    final TupleResultDomainValueData< ? , ? > valueData = getValueData();
    if( valueData == null )
      return;

    final List<Point> path = new ArrayList<Point>();

    valueData.open();

    final Object[] domainValues = valueData.getDomainValues();
    final Object[] targetValues = valueData.getTargetValues();

    if( domainValues.length > 0 && targetValues.length > 0 )
    {
      for( int i = 0; i < domainValues.length; i++ )
      {
        final Object domainValue = domainValues[i];
        final Object targetValue = targetValues[i];

        // we have to check if all values are correct - an incorrect value means a null value - the axis would return 0
        // in that case
        if( domainValue != null && targetValue != null )
        {
          final Point screen = getCoordinateMapper().logicalToScreen( domainValue, targetValue );
          path.add( screen );
          if( i < domainValues.length - 1 && domainValues[i + 1] != null && targetValues[i + 1] != null )
          {
            final Point next = getCoordinateMapper().logicalToScreen( domainValues[i + 1], targetValues[i + 1] );
            if( next.y != screen.y )
              path.add( new Point( next.x, screen.y ) );
          }
        }
      }
    }
    paint( gc, path.toArray( new Point[] {} ) );
  }

}

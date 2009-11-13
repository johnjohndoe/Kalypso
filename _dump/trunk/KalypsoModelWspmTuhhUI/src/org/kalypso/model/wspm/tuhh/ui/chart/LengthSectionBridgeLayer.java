package org.kalypso.model.wspm.tuhh.ui.chart;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.chart.ext.observation.data.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.layer.TupleResultLineLayer;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.framework.model.data.IDataOperator;
import de.openali.odysseus.chart.framework.model.figure.impl.FullRectangleFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.impl.AreaStyle;
import de.openali.odysseus.chart.framework.model.style.impl.ColorFill;

public class LengthSectionBridgeLayer extends TupleResultLineLayer
{
  public LengthSectionBridgeLayer( final TupleResultDomainValueData< ? , ? > data, final ILineStyle lineStyle, final IPointStyle pointStyle )
  {
    super( data, lineStyle, pointStyle );

  }

  @Override
  @SuppressWarnings("unchecked")
  public void paint( final GC gc )
  {
    if( m_data == null )
      return;

    final List<Point> path = new ArrayList<Point>();

    m_data.open();

    final TupleResult result = m_data.getResult();

    final Object[] domainValues = m_data.getDomainValues();
    final Object[] targetValues = m_data.getTargetValues();

    final int iUK = m_data.getResult().indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK );
    final Object[] uKValues = iUK < 0 ? new Object[] {} : ProfilUtil.getValuesFor( result.toArray( new IRecord[] {} ), result.getComponent( iUK ) );

    if( domainValues.length > 0 && targetValues.length > 0 && uKValues.length > 0 )
    {
      final IAxis domainAxis = getDomainAxis();
      final IAxis targetAxis = getTargetAxis();
      final IDataOperator dopDomain = domainAxis.getDataOperator( domainAxis.getDataClass() );
      final IDataOperator dopTarget = targetAxis.getDataOperator( targetAxis.getDataClass() );

      if( dopDomain == null || dopTarget == null )
        return;

      final PointFigure pf = getPointFigure();
      final IPointStyle ps = pf.getStyle();
      final FullRectangleFigure rf = new FullRectangleFigure();
      rf.setStyle( new AreaStyle( new ColorFill( ps.getInlineColor() ), ps.getAlpha(), ps.getStroke(), true ) );
      for( int i = 0; i < domainValues.length; i++ )
      {
        final Number domainValue = dopDomain.logicalToNumeric( domainValues[i] );
        final Number domainValLeft = i > 0 ? dopDomain.logicalToNumeric( domainValues[i - 1] ) : domainValue;
        final Number domainValRight = i < domainValues.length - 1 ? dopDomain.logicalToNumeric( domainValues[i + 1] ) : domainValue;
        final Number targetValue = dopTarget.logicalToNumeric( targetValues[i] );
        final Number uKValue = dopTarget.logicalToNumeric( uKValues[i] );
       if( domainValue != null && targetValue != null && uKValue != null )
        {
          final Point OKLeft = getCoordinateMapper().numericToScreen( (Double) domainValLeft + ((Double) domainValue - (Double) domainValLeft) / 2.0, targetValue );
          final Point UKRight = getCoordinateMapper().numericToScreen( (Double) domainValue + ((Double) domainValRight - (Double) domainValue) / 2.0, uKValue );
          path.add( OKLeft );

          rf.setRectangle(new Rectangle( OKLeft.x, OKLeft.y, UKRight.x-OKLeft.x,  UKRight.y-OKLeft.y ));
          rf.paint( gc );
        }
      }
    }
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.ITooltipChartLayer#getHover(org.eclipse.swt.graphics.Point)
   */
  @Override
  protected final String getTooltip( final int index )
  {

    return m_data.getResult().get( index ).toString();
  }

}

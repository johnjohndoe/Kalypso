package org.kalypso.model.wspm.tuhh.ui.chart;

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
  public void paint( final GC gc )
  {
    if( m_data == null )
      return;
    m_data.open();

    final PointFigure pf = getPointFigure();
    final IPointStyle ps = pf.getStyle();
    final FullRectangleFigure rf = new FullRectangleFigure();
    rf.setStyle( new AreaStyle( new ColorFill( ps.getInlineColor() ), ps.getAlpha(), ps.getStroke(), true ) );
    for( int i = 0; i < m_data.getObservation().getResult().size(); i++ )
    {
      final Rectangle rect = getScreenRect( i );
      if( rect != null )
      {
        rf.setRectangle( rect );
        rf.paint( gc );
      }
    }
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.ITooltipChartLayer#getHover(org.eclipse.swt.graphics.Point)
   */
  @Override
  protected final String getTooltip( final int index )
  {
    final TupleResult tr = m_data.getObservation().getResult();
    final int targetOKComponentIndex = tr.indexOfComponent( m_data.getTargetComponentName() );
    final int targetUKComponentIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK );
    final String targetOKComponentLabel = tr.getComponent( targetOKComponentIndex ).getName();
    final String targetUKComponentLabel = tr.getComponent( targetUKComponentIndex ).getName();
    final String targetOKComponentUnit = tr.getComponent( targetOKComponentIndex ).getUnit();
    final String targetUKComponentUnit = tr.getComponent( targetUKComponentIndex ).getUnit();
    final Object uk = tr.get( index ).getValue( targetUKComponentIndex );
    final Object ok = tr.get( index ).getValue( targetOKComponentIndex );

    return String.format( TOOLTIP_FORMAT, new Object[] { "max. " + targetOKComponentLabel, ok, targetOKComponentUnit, "min. " + targetUKComponentLabel, uk, targetUKComponentUnit } );
  }

  @Override
  protected Rectangle getHoverRect( final Point screen, final int index )
  {
    return getScreenRect( index );
  }

  private final Rectangle getScreenRect( final int i )
  {
    final TupleResult result = m_data.getObservation().getResult();
    final int iUK = m_data.getObservation().getResult().indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK );
    final Object[] domainValues = m_data.getDomainValues();
    final Object[] targetValues = m_data.getTargetValues();
    final Object[] uKValues = iUK < 0 ? new Object[] {} : ProfilUtil.getValuesFor( result.toArray( new IRecord[] {} ), result.getComponent( iUK ) );

    final IAxis domainAxis = getDomainAxis();
    final IAxis targetAxis = getTargetAxis();
    final IDataOperator dopDomain = domainAxis.getDataOperator( domainAxis.getDataClass() );
    final IDataOperator dopTarget = targetAxis.getDataOperator( targetAxis.getDataClass() );
    if( dopDomain == null || dopTarget == null )
      return null;
    final Number domainValue = dopDomain.logicalToNumeric( domainValues[i] );
    final Number domainValLeft = i > 0 ? dopDomain.logicalToNumeric( domainValues[i - 1] ) : domainValue;
    final Number domainValRight = i < domainValues.length - 1 ? dopDomain.logicalToNumeric( domainValues[i + 1] ) : domainValue;
    final Number targetValue = dopTarget.logicalToNumeric( targetValues[i] );
    final Number uKValue = dopTarget.logicalToNumeric( uKValues[i] );
    if( domainValue != null && targetValue != null && uKValue != null &&domainValRight!=null&&domainValLeft!=null)
    {
      final Point OKLeft = getCoordinateMapper().numericToScreen( (Double) domainValLeft + ((Double) domainValue - (Double) domainValLeft) / 2.0, targetValue );
      final Point UKRight = getCoordinateMapper().numericToScreen( (Double) domainValue + ((Double) domainValRight - (Double) domainValue) / 2.0, uKValue );
      return new Rectangle( OKLeft.x, OKLeft.y, UKRight.x - OKLeft.x, UKRight.y - OKLeft.y );
    }
    return null;
  }
}

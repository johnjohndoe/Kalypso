package org.kalypso.model.wspm.tuhh.ui.chart;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.chart.ext.observation.data.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.layer.TupleResultLineLayer;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.framework.model.figure.impl.FullRectangleFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
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
    final IObservation<TupleResult> obs = m_data.getObservation();
    if( obs == null )
      return;
    final PointFigure pf = getPointFigure();
    final IPointStyle ps = pf.getStyle();
    final FullRectangleFigure rf = new FullRectangleFigure();
    rf.setStyle( new AreaStyle( new ColorFill( ps.getInlineColor() ), ps.getAlpha(), ps.getStroke(), true ) );
    for( int i = 0; i < obs.getResult().size(); i++ )
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
    final IRecord rec = tr.get( index );
    final int targetOKComponentIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK );
    final int targetUKComponentIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK );
    final int commentIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_TEXT );
    final String targetOKComponentLabel = ComponentUtilities.getComponentLabel( tr.getComponent( targetOKComponentIndex ) );
    final String targetUKComponentLabel = ComponentUtilities.getComponentLabel( tr.getComponent( targetUKComponentIndex ) );
    final Double uk = ProfilUtil.getDoubleValueFor( targetUKComponentIndex, rec );
    final Object ok = ProfilUtil.getDoubleValueFor( targetOKComponentIndex, rec );
    if( commentIndex < 0 )
      return String.format( "max. %-12s %.4f %nmin. %-12s %.4f", new Object[] { targetOKComponentLabel, ok, targetUKComponentLabel, uk } );//$NON-NLS-1$
    return String.format( "max. %-12s %.4f %nmin. %-12s %.4f%n%s", new Object[] { targetOKComponentLabel, ok, targetUKComponentLabel, uk, tr.get( index ).getValue( commentIndex ) } );//$NON-NLS-1$

  }

  @Override
  protected Rectangle getHoverRect( final Point screen, final int index )
  {
    return getScreenRect( index );
  }

  private final Rectangle getScreenRect( final int i )
  {
    final TupleResult result = m_data.getObservation().getResult();
    final IRecord record = result.get( i );
    final int iUK = m_data.getObservation().getResult().indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK );
    final int iOK = m_data.getObservation().getResult().indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK );
    final int iST = m_data.getObservation().getResult().indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_STATION );
    final Double uK = ProfilUtil.getDoubleValueFor( iUK, record );
    final Double oK = ProfilUtil.getDoubleValueFor( iOK, record );
    final Double sT = ProfilUtil.getDoubleValueFor( iST, record );
    if( uK.isNaN() || oK.isNaN() || sT.isNaN() )
      return null;
    final Point pUK = getCoordinateMapper().numericToScreen( sT, uK );
    final Point pOK = getCoordinateMapper().numericToScreen( sT, oK );
    return new Rectangle( pOK.x - 5, pOK.y, 10, pUK.y - pOK.y );
  }
}

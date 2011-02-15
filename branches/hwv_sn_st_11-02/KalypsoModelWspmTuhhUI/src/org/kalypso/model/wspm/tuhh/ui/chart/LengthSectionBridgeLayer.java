package org.kalypso.model.wspm.tuhh.ui.chart;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.chart.ext.observation.data.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.layer.TupleResultLineLayer;
import org.kalypso.contribs.eclipse.swt.graphics.RectangleUtils;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.framework.model.figure.impl.FullRectangleFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ILayerProvider;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.impl.AreaStyle;
import de.openali.odysseus.chart.framework.model.style.impl.ColorFill;

public class LengthSectionBridgeLayer extends TupleResultLineLayer
{
  public LengthSectionBridgeLayer( final ILayerProvider provider, final TupleResultDomainValueData< ? , ? > data, final ILineStyle lineStyle, final IPointStyle pointStyle )
  {
    super( provider, data, lineStyle, pointStyle );
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

    final int stationIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_STATION );
    final int okIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK );
    final int ukIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK );
    final int commentIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_TEXT );
    final int bridgeWidthIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_BRIDGE_WIDTH );

    final String stationLabel = ComponentUtilities.getComponentLabel( tr.getComponent( stationIndex ) );
    final String okLabel = "max. " + ComponentUtilities.getComponentLabel( tr.getComponent( okIndex ) ); //$NON-NLS-1$
    final String ukLabel = "min. " + ComponentUtilities.getComponentLabel( tr.getComponent( ukIndex ) ); //$NON-NLS-1$
    final String widthLabel = ComponentUtilities.getComponentLabel( tr.getComponent( bridgeWidthIndex ) );

    final Object station = rec.getValue( stationIndex );
    final Double uk = ProfilUtil.getDoubleValueFor( ukIndex, rec );
    final Double ok = ProfilUtil.getDoubleValueFor( okIndex, rec );
    final Double bw = ProfilUtil.getDoubleValueFor( bridgeWidthIndex, rec );

    final ProfilChartTooltip tooltip = new ProfilChartTooltip();
    tooltip.add( stationLabel, station );
    tooltip.add( okLabel, ok );
    tooltip.add( ukLabel, uk );
    tooltip.add( widthLabel, bw );

    if( commentIndex >= 0 )
    {
      final String comment = (String) tr.get( index ).getValue( commentIndex );
      tooltip.add( comment, null );
    }

    return tooltip.toString();
  }

  @Override
  public EditInfo getHover( final Point pos )
  {
    if( !isVisible() )
      return null;
    for( int i = 0; i < m_data.getDomainValues().length; i++ )
    {
      final Rectangle hover = getScreenRect( i );
      if( hover != null && hover.contains( pos ) )
        return new EditInfo( this, null, null, i, getTooltip( i ), RectangleUtils.getCenterPoint( hover ) );
    }
    return null;
  }

  private final Rectangle getScreenRect( final int i )
  {
    final TupleResult result = m_data.getObservation().getResult();
    final IRecord record = result.get( i );

    final Double uK = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK, record );
    final Double oK = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK, record );
    final Double sT = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_STATION, record );
    final Double bR = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_BRIDGE_WIDTH, record );
    if( bR.isNaN() || uK.isNaN() || sT.isNaN() || oK.isNaN() )
      return null;

    final double startBridge = sT + bR / 2000;
    final double endBridge = sT - bR / 2000;

    final Point bottomLeft = getCoordinateMapper().numericToScreen( startBridge, uK );
    final Point topRight = getCoordinateMapper().numericToScreen( endBridge, oK );
    return RectangleUtils.createNormalizedRectangle( bottomLeft, topRight );
  }
}

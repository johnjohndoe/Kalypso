package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.chart.ext.observation.data.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.layer.TupleResultLineLayer;
import org.kalypso.contribs.eclipse.swt.graphics.RectangleUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.ui.view.chart.layer.wsp.TooltipFormatter;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.framework.model.figure.impl.FullRectangleFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ILayerProvider;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.IStyleSet;
import de.openali.odysseus.chart.framework.model.style.impl.AreaStyle;
import de.openali.odysseus.chart.framework.model.style.impl.ColorFill;

public class LengthSectionBridgeLayer extends TupleResultLineLayer
{
  public LengthSectionBridgeLayer( final ILayerProvider provider, final TupleResultDomainValueData< ? , ? > data, final IStyleSet styleSet )
  {
    super( provider, data, styleSet );
  }

  @Override
  public void paint( final GC gc )
  {
    final TupleResultDomainValueData< ? , ? > valueData = getValueData();
    if( valueData == null )
      return;

    valueData.open();
    final IObservation<TupleResult> obs = valueData.getObservation();
    if( obs == null )
      return;
    final FullRectangleFigure rf = new FullRectangleFigure();
    final IPointStyle ps = getPointStyle();
    rf.setStyle( new AreaStyle( new ColorFill( ps.getInlineColor() ), ps.getAlpha(), ps.getStroke(), true ) );
    for( int i = 0; i < obs.getResult().size(); i++ )
    {
      final Rectangle rect = getScreenRect( i );
      if( rect != null )
      {
        if( rect.width < 1 )
          rect.width = 1;
        rf.setRectangle( rect );
        rf.paint( gc );
      }
    }
  }

  @Override
  protected final String getTooltip( final int index )
  {
    final TupleResultDomainValueData< ? , ? > valueData = getValueData();
    final TupleResult tr = valueData.getObservation().getResult();
    final IRecord rec = tr.get( index );

    final int stationIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION );
    final int okIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK );
    final int ukIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK );
    final int commentIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_TEXT );
    final int bridgeWidthIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_WIDTH );

    final String stationLabel = ComponentUtilities.getComponentLabel( tr.getComponent( stationIndex ) );
    final String okLabel = "max. " + ComponentUtilities.getComponentLabel( tr.getComponent( okIndex ) ); //$NON-NLS-1$
    final String ukLabel = "min. " + ComponentUtilities.getComponentLabel( tr.getComponent( ukIndex ) ); //$NON-NLS-1$
    final String widthLabel = ComponentUtilities.getComponentLabel( tr.getComponent( bridgeWidthIndex ) );

    final Object station = rec.getValue( stationIndex );
    final Double uk = ProfilUtil.getDoubleValueFor( ukIndex, rec );
    final Double ok = ProfilUtil.getDoubleValueFor( okIndex, rec );
    final Double bw = ProfilUtil.getDoubleValueFor( bridgeWidthIndex, rec );

    final String comment = commentIndex >= 0 ? (String) tr.get( index ).getValue( commentIndex ) : ""; //$NON-NLS-1$

    final TooltipFormatter tooltip = new TooltipFormatter( comment );
    tooltip.addLine( stationLabel, station.toString() );
    tooltip.addLine( okLabel, ok.toString() );
    tooltip.addLine( ukLabel, uk.toString() );
    tooltip.addLine( widthLabel, bw.toString() );

    return tooltip.format();
  }

  @Override
  public EditInfo getHover( final Point pos )
  {
    final TupleResultDomainValueData< ? , ? > valueData = getValueData();
    if( !isVisible() )
      return null;
    for( int i = 0; i < valueData.getDomainValues().length; i++ )
    {
      final Rectangle hover = getScreenRect( i );
      if( hover != null && hover.contains( pos ) )
        return new EditInfo( this, null, null, i, getTooltip( i ), RectangleUtils.getCenterPoint( hover ) );
    }
    return null;
  }

  private Rectangle getScreenRect( final int i )
  {
    final TupleResultDomainValueData< ? , ? > valueData = getValueData();
    final TupleResult result = valueData.getObservation().getResult();
    final IRecord record = result.get( i );

    final Double uK = ProfilUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK, record );
    final Double oK = ProfilUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK, record );
    final Double sT = ProfilUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION, record );
    final Double bR = ProfilUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_WIDTH, record );
    if( bR.isNaN() || uK.isNaN() || sT.isNaN() || oK.isNaN() )
      return null;

    final double startBridge = sT + bR / 2000;
    final double endBridge = sT - bR / 2000;

    final ICoordinateMapper coordinateMapper = getCoordinateMapper();

    final Point bottomLeft = coordinateMapper.numericToScreen( startBridge, uK );
    final Point topRight = coordinateMapper.numericToScreen( endBridge, oK );
    return RectangleUtils.createNormalizedRectangle( bottomLeft, topRight );
  }
}

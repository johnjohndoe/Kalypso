package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.chart.ext.observation.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.TupleResultLineLayer;
import org.kalypso.contribs.eclipse.swt.graphics.RectangleUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.ext.base.layer.TooltipFormatter;
import de.openali.odysseus.chart.framework.model.figure.IPaintable;
import de.openali.odysseus.chart.framework.model.figure.impl.FullRectangleFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ILayerProvider;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.IStyleSet;
import de.openali.odysseus.chart.framework.model.style.impl.AreaStyle;
import de.openali.odysseus.chart.framework.model.style.impl.ColorFill;
import de.openali.odysseus.chart.framework.util.img.ChartImageInfo;

//FIXME: why do we extend from TupleResultLineLayer -> implementation is totally different!
public class LengthSectionBridgeLayer extends TupleResultLineLayer
{
  public LengthSectionBridgeLayer( final ILayerProvider provider, final TupleResultDomainValueData< ? , ? > data, final IStyleSet styleSet )
  {
    super( provider, data, styleSet );
  }

  @Override
  public void paint( final GC gc,final ChartImageInfo chartImageInfo,  final IProgressMonitor monitor )
  {
    final TupleResultDomainValueData< ? , ? > valueData = getValueData();
    if( valueData == null )
      return;

    final IObservation<TupleResult> observation = valueData.getObservation();
    if( observation == null )
      return;

    /* recreate hover info on every paint */
    clearInfoIndex();

    final FullRectangleFigure rf = new FullRectangleFigure();
    final IPointStyle ps = getPointStyle();
    rf.setStyle( new AreaStyle( new ColorFill( ps.getInlineColor() ), ps.getAlpha(), ps.getStroke(), true ) );

    final TupleResult result = observation.getResult();

    for( int i = 0; i < result.size(); i++ )
    {
      final IRecord record = result.get( i );

      final Rectangle rect = getScreenRect( record );
      if( rect != null )
      {
        if( rect.width < 1 )
          rect.width = 1;

        rf.setRectangle( rect );
        rf.paint( gc );

        addInfo( rect, record, i );
      }
    }
  }

  private void addInfo( final Rectangle bounds, final IRecord record, final int recordIndex )
  {
    // FIXME: lets have a nice figure!
    final IPaintable hoverFigure = null;

    final String tooltip = getTooltip( record );

    final EditInfo info = new EditInfo( this, hoverFigure, null, recordIndex, tooltip, null );
    addInfoElement( bounds, info );
  }

  @Override
  protected final String getTooltip( final IRecord record )
  {
    final TupleResult tr = record.getOwner();

    final int stationIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION );
    final int okIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK );
    final int ukIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK );
    final int commentIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_TEXT );
    final int bridgeWidthIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_WIDTH );

    final String stationLabel = ComponentUtilities.getComponentLabel( tr.getComponent( stationIndex ) );
    final String okLabel = Messages.getString("LengthSectionBridgeLayer_0") + ComponentUtilities.getComponentLabel( tr.getComponent( okIndex ) ); //$NON-NLS-1$
    final String ukLabel = Messages.getString("LengthSectionBridgeLayer_1") + ComponentUtilities.getComponentLabel( tr.getComponent( ukIndex ) ); //$NON-NLS-1$
    final String widthLabel = ComponentUtilities.getComponentLabel( tr.getComponent( bridgeWidthIndex ) );

    final Object station = record.getValue( stationIndex );
    final Double uk = ProfileUtil.getDoubleValueFor( ukIndex, record );
    final Double ok = ProfileUtil.getDoubleValueFor( okIndex, record );
    final Double bw = ProfileUtil.getDoubleValueFor( bridgeWidthIndex, record );

    final String comment = commentIndex >= 0 ? (String)record.getValue( commentIndex ) : ""; //$NON-NLS-1$

    final TooltipFormatter tooltip = new TooltipFormatter( comment );
    tooltip.addLine( stationLabel, station.toString() );
    tooltip.addLine( okLabel, ok.toString() );
    tooltip.addLine( ukLabel, uk.toString() );
    tooltip.addLine( widthLabel, bw.toString() );

    return tooltip.format();
  }

  @Override
  public String getTitle( )
  {
    final String title = super.getTitle();

    final int index = title.indexOf( '(' ); //$NON-NLS-1$ // remove '(Oberkante)' from String
    if( index > 0 )
      return StringUtils.chomp( title.substring( 0, index - 1 ) );

    return title;
  }

  private Rectangle getScreenRect( final IRecord record )
  {
    final Double unterkante = ProfileUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK, record );
    final Double oberkante = ProfileUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK, record );
    final Double station = ProfileUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION, record );
    final Double bridgeWidth = ProfileUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_WIDTH, record );
    if( bridgeWidth.isNaN() || unterkante.isNaN() || station.isNaN() || oberkante.isNaN() )
      return null;

    final ICoordinateMapper coordinateMapper = getCoordinateMapper();

    /** hack: unknown fliessrichtung - but drawing is always in fliessrichtung */
    final Point p0 = coordinateMapper.numericToScreen( station, unterkante );
    final Point pDiff = coordinateMapper.numericToScreen( (station + bridgeWidth / 1000.0), oberkante );

    final int x1 = p0.x + Math.abs( p0.x - pDiff.x );

    return RectangleUtils.createNormalizedRectangle( p0, new Point( x1, pDiff.y ) );
  }
}
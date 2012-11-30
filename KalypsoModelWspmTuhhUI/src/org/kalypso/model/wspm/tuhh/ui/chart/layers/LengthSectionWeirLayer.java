package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.chart.ext.observation.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.TupleResultLineLayer;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.framework.model.figure.IPaintable;
import de.openali.odysseus.chart.framework.model.figure.impl.FullRectangleFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ILayerProvider;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.IStyleSet;
import de.openali.odysseus.chart.framework.model.style.impl.AreaStyle;
import de.openali.odysseus.chart.framework.model.style.impl.ColorFill;
import de.openali.odysseus.chart.framework.util.img.ChartImageInfo;

//FIXME: why do we extend from TupleResultLineLayer -> implementation is totally different!
public class LengthSectionWeirLayer extends TupleResultLineLayer
{
  public LengthSectionWeirLayer( final ILayerProvider provider, final TupleResultDomainValueData< ? , ? > data, final IStyleSet styleSet )
  {
    super( provider, data, styleSet );
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

  private Rectangle getScreenRect( final IRecord record )
  {
    final Double oK = ProfileUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_WEIR_OK, record );
    final Double sT = ProfileUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION, record );
    final Double uK = ProfileUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_GROUND, record );
    if( uK.isNaN() || oK.isNaN() || sT.isNaN() || getCoordinateMapper() == null )
      return null;

    final Point pOK = getCoordinateMapper().numericToScreen( sT, oK );
    final Point pUK = getCoordinateMapper().numericToScreen( sT, uK );
    return new Rectangle( pOK.x - 1, pOK.y, 3, pUK.y - pOK.y );
  }

  @Override
  protected final String getTooltip( final IRecord record )
  {
    final TupleResult tr = record.getOwner();

    final int targetOKComponentIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_WEIR_OK );
    final String targetOKComponentLabel = ComponentUtilities.getComponentLabel( tr.getComponent( targetOKComponentIndex ) );
    final Double ok = ProfileUtil.getDoubleValueFor( targetOKComponentIndex, record );

    final int commentIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_TEXT );
    if( commentIndex < 0 )
      return String.format( "%-12s %.4f", new Object[] { targetOKComponentLabel, ok } );//$NON-NLS-1$

    final Object comment = record.getValue( commentIndex );
    return String.format( "%-12s %.4f%n%s", new Object[] { targetOKComponentLabel, ok, comment } );//$NON-NLS-1$
  }
}
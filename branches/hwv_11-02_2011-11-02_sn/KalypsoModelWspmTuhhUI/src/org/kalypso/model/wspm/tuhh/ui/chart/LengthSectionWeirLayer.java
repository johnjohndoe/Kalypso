package org.kalypso.model.wspm.tuhh.ui.chart;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.chart.ext.observation.data.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.layer.TupleResultLineLayer;
import org.kalypso.contribs.eclipse.swt.graphics.RectangleUtils;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
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

public class LengthSectionWeirLayer extends TupleResultLineLayer
{
  /**
   * @see org.kalypso.chart.ext.observation.layer.TupleResultLineLayer#getTitle()
   */

  public LengthSectionWeirLayer( final ILayerProvider provider, final TupleResultDomainValueData< ? , ? > data, final ILineStyle lineStyle, final IPointStyle pointStyle )
  {
    super( provider, data, lineStyle, pointStyle );

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

  @Override
  protected final String getTooltip( final int index )
  {
    final TupleResult tr = m_data.getObservation().getResult();
    final IRecord rec = tr.get( index );
    final int targetOKComponentIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_WEIR_OK );
    final int commentIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_TEXT );
    final String targetOKComponentLabel = ComponentUtilities.getComponentLabel( tr.getComponent( targetOKComponentIndex ) );
    final Double ok = ProfilUtil.getDoubleValueFor( targetOKComponentIndex, rec );
    if( commentIndex < 0 )
      return String.format( "%-12s %.4f", new Object[] { targetOKComponentLabel, ok } );//$NON-NLS-1$
    return String.format( "%-12s %.4f%n%s", new Object[] { targetOKComponentLabel, ok, tr.get( index ).getValue( commentIndex ) } );//$NON-NLS-1$

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

  private final Rectangle getScreenRect( final int i )
  {
    final TupleResult result = m_data.getObservation().getResult();
    final IRecord record = result.get( i );
    final Double oK = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_WEIR_OK, record );
    final Double sT = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_STATION, record );
    final Double uK = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_GROUND, record );
    if( uK.isNaN() || oK.isNaN() || sT.isNaN()||getCoordinateMapper()==null )
      return null;
    final Point pOK = getCoordinateMapper().numericToScreen( sT, oK );
    final Point pUK = getCoordinateMapper().numericToScreen( sT, uK );
    return new Rectangle( pOK.x - 1, pOK.y, 3, pUK.y - pOK.y );
  }

}

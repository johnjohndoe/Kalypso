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
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.data.impl.DataRange;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;

public class LengthSectionCulvertLayer extends TupleResultLineLayer
{
  /**
   * @see org.kalypso.chart.ext.observation.layer.TupleResultLineLayer#getTitle()
   */

  public LengthSectionCulvertLayer( final TupleResultDomainValueData< ? , ? > data, final ILineStyle lineStyle, final IPointStyle pointStyle )
  {

    super( data, lineStyle, pointStyle );
  }

  /**
   * @see org.kalypso.chart.ext.observation.layer.TupleResultLineLayer#getTargetRange()
   */
  @Override
  public IDataRange<Number> getTargetRange( )
  {
    final IObservation<TupleResult> obs = m_data.getObservation();
    return getDataRange( obs == null ? null : obs.getResult(), IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_GROUND );
  }

  private final IDataRange<Number> getDataRange( final TupleResult tupleResult, final String id )
  {
    final IRecord[] record = tupleResult == null ? new IRecord[] {} : tupleResult.toArray( new IRecord[] {} );
    final IComponent[] components = tupleResult == null ? null : tupleResult.getComponents();
    final IComponent component = ProfilUtil.getComponentForID( components, id );
    Double min = ProfilUtil.getSectionMinValueFor( record, component );
    Double max = ProfilUtil.getSectionMaxValueFor( record, component );
    return new DataRange<Number>( min, max );
  }

  @Override
  protected final String getTooltip( final int index )
  {
    final TupleResult tr = m_data.getObservation().getResult();
    final IRecord rec = tr.get( index );
    final int targetOKComponentIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_ROHR_DN );
    final int stationIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_STATION );
    final String targetOKComponentLabel = ComponentUtilities.getComponentLabel( tr.getComponent( targetOKComponentIndex ) );
    final String stationComponentLabel = ComponentUtilities.getComponentLabel( tr.getComponent( stationIndex ) );
    final Double dn = ProfilUtil.getDoubleValueFor( targetOKComponentIndex, rec );
    final Double ds = ProfilUtil.getDoubleValueFor( stationIndex, rec );
    return String.format( "%-12s %.4f%n%-12s %.4f", new Object[] { stationComponentLabel, ds, targetOKComponentLabel, dn } );//$NON-NLS-1$
  }

  @Override
  public void paint( final GC gc )
  {
    if( m_data == null )
      return;
    m_data.open();
    for( int i = 0; i < m_data.getObservation().getResult().size(); i++ )
    {
      final Rectangle rect = getScreenRect( i );
      if( rect != null )
      {
        getPointFigure().setPoints( new Point[] { RectangleUtils.getCenterPoint( rect ) } );
        getPointFigure().getStyle().setWidth( rect.width );
        getPointFigure().getStyle().setHeight( rect.height );
        getPointFigure().paint( gc );
      }
    }
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
    final int iDN = m_data.getObservation().getResult().indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_ROHR_DN );
    final int iUK = m_data.getObservation().getResult().indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_GROUND );
    final int iST = m_data.getObservation().getResult().indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_STATION );
    final Double dN = ProfilUtil.getDoubleValueFor( iDN, record );
    final Double sT = ProfilUtil.getDoubleValueFor( iST, record );
    final Double uK = ProfilUtil.getDoubleValueFor( iUK, record );
    if( uK.isNaN() || dN.isNaN() || sT.isNaN() )
      return null;
    final Point pOK = getCoordinateMapper().numericToScreen( sT, uK );
    final Point pBase = getCoordinateMapper().numericToScreen( 0, 0 );
    final Point pDN = getCoordinateMapper().numericToScreen( dN / 1000, dN );
    final int dx = pBase.x - pDN.x;
    final int dy = pBase.y - pDN.y;
    return new Rectangle( pOK.x - dx / 2, pOK.y - dy, dx, dy );
  }

}

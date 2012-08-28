package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.chart.ext.observation.data.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.layer.TupleResultLineLayer;
import org.kalypso.contribs.eclipse.swt.graphics.RectangleUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.framework.model.data.DataRange;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ILayerProvider;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.IStyleSet;
import de.openali.odysseus.chart.framework.util.img.ChartImageInfo;

public class LengthSectionCulvertLayer extends TupleResultLineLayer
{
  public LengthSectionCulvertLayer( final ILayerProvider provider, final TupleResultDomainValueData< ? , ? > data, final IStyleSet styleSet )
  {
    super( provider, data, styleSet );
  }

  /**
   * @see org.kalypso.chart.ext.observation.layer.TupleResultLineLayer#getTargetRange()
   */
  @Override
  public IDataRange< ? > getTargetRange( final IDataRange< ? > domainIntervall )
  {
    final TupleResultDomainValueData< ? , ? > valueData = getValueData();
    final IObservation<TupleResult> obs = valueData.getObservation();
    return getDataRange( obs == null ? null : obs.getResult(), IWspmConstants.LENGTH_SECTION_PROPERTY_GROUND );
  }

  private IDataRange<Number> getDataRange( final TupleResult tupleResult, final String id )
  {
    final IRecord[] record = tupleResult == null ? new IRecord[] {} : tupleResult.toArray( new IRecord[] {} );
    final IComponent[] components = tupleResult == null ? null : tupleResult.getComponents();
    final IComponent component = ProfilUtil.getComponentForID( components, id );
    final Number min = ProfilUtil.getSectionMinValueFor( record, component );
    final Number max = ProfilUtil.getSectionMaxValueFor( record, component );
    return DataRange.create( min, max );
  }

  @Override
  protected final String getTooltip( final int index )
  {
    final TupleResultDomainValueData< ? , ? > valueData = getValueData();
    final TupleResult tr = valueData.getObservation().getResult();
    final IRecord rec = tr.get( index );
    final int targetOKComponentIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_ROHR_DN );
    final int stationIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION );
    final String targetOKComponentLabel = ComponentUtilities.getComponentLabel( tr.getComponent( targetOKComponentIndex ) );
    final String stationComponentLabel = ComponentUtilities.getComponentLabel( tr.getComponent( stationIndex ) );
    final Double dn = ProfilUtil.getDoubleValueFor( targetOKComponentIndex, rec );
    final Double ds = ProfilUtil.getDoubleValueFor( stationIndex, rec );
    return String.format( "%-12s %.4f%n%-12s %.4f", new Object[] { stationComponentLabel, ds, targetOKComponentLabel, dn } );//$NON-NLS-1$
  }

  @Override
  public void paint( final GC gc, final ChartImageInfo chartImageInfo, final IProgressMonitor monitor )
  {
    final TupleResultDomainValueData< ? , ? > valueData = getValueData();
    if( valueData == null )
      return;
    valueData.open();

    // FIXME: painting an ellipse here makes no sense!
    // FIXME: wir brauchen mehr Informationen �ber die verdohlungsl�nge
    for( int i = 0; i < valueData.getObservation().getResult().size(); i++ )
    {
      final Rectangle rect = getScreenRect( i );
      if( rect != null )
      {
        final PointFigure ps = new PointFigure();
        ps.setPoints( new Point[] { RectangleUtils.getCenterPoint( rect ) } );
        ps.getStyle().setWidth( rect.width );
        ps.getStyle().setHeight( rect.height );
        ps.paint( gc );
      }
    }
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
    final Double dN = ProfilUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_ROHR_DN, record );
    final Double sT = ProfilUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION, record );
    final Double uK = ProfilUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_GROUND, record );
    if( uK.isNaN() || dN.isNaN() || sT.isNaN() )
      return null;

    final ICoordinateMapper coordinateMapper = getCoordinateMapper();

    final Point pOK = coordinateMapper.numericToScreen( sT, uK );
    final Point pBase = coordinateMapper.numericToScreen( 0, 0 );
    final Point pDN = coordinateMapper.numericToScreen( dN / 1000, dN );
    final int dx = pBase.x - pDN.x;
    final int dy = pBase.y - pDN.y;
    return new Rectangle( pOK.x - dx / 2, pOK.y - dy, dx, dy );
  }

}

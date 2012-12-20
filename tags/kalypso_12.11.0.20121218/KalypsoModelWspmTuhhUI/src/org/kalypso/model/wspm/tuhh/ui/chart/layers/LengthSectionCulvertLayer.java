package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.chart.ext.observation.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.TupleResultLineLayer;
import org.kalypso.contribs.eclipse.swt.graphics.RectangleUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.framework.model.data.DataRange;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.figure.IPaintable;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ILayerProvider;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.IStyleSet;
import de.openali.odysseus.chart.framework.util.img.ChartImageInfo;

// FIXME: why do we extend from TupleResultLineLayer -> implementation is totally different!
public class LengthSectionCulvertLayer extends TupleResultLineLayer
{
  public LengthSectionCulvertLayer( final ILayerProvider provider, final TupleResultDomainValueData< ? , ? > data, final IStyleSet styleSet )
  {
    super( provider, data, styleSet );
  }

  @Override
  public IDataRange<Double> getTargetRange( final IDataRange<Double> domainIntervall )
  {
    final TupleResultDomainValueData< ? , ? > valueData = getValueData();
    final IObservation<TupleResult> obs = valueData.getObservation();

    // TODO: nonsense, give our own range, not that of ground

    return getDataRange( obs == null ? null : obs.getResult(), IWspmConstants.LENGTH_SECTION_PROPERTY_GROUND );
  }

  private IDataRange<Double> getDataRange( final TupleResult tupleResult, final String id )
  {
    final IRecord[] record = tupleResult == null ? new IRecord[] {} : tupleResult.toArray( new IRecord[] {} );
    final IComponent[] components = tupleResult == null ? null : tupleResult.getComponents();
    final IComponent component = ProfileUtil.getComponentForID( components, id );
    final Double min = ProfileUtil.getSectionMinValueFor( record, component );
    final Double max = ProfileUtil.getSectionMaxValueFor( record, component );
    return new DataRange<>( min, max );
  }

  @Override
  protected String getTooltip( final IRecord record )
  {
    final TupleResult tr = record.getOwner();

    final int targetOKComponentIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_ROHR_DN );
    final int stationIndex = tr.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION );

    final String targetOKComponentLabel = ComponentUtilities.getComponentLabel( tr.getComponent( targetOKComponentIndex ) );
    final String stationComponentLabel = ComponentUtilities.getComponentLabel( tr.getComponent( stationIndex ) );

    // FIXME: better tooltip

    final Double dn = ProfileUtil.getDoubleValueFor( targetOKComponentIndex, record );
    final Double ds = ProfileUtil.getDoubleValueFor( stationIndex, record );
    return String.format( "%-12s %.4f%n%-12s %.4f", new Object[] { stationComponentLabel, ds, targetOKComponentLabel, dn } );//$NON-NLS-1$
  }

  @Override
  public void paint( final GC gc, final ChartImageInfo chartImageInfo, final IProgressMonitor monitor )
  {
    final TupleResultDomainValueData< ? , ? > valueData = getValueData();
    if( valueData == null )
      return;

    // FIXME: painting an ellipse here makes no sense!
    // FIXME: wir brauchen mehr Informationen über die verdohlungslänge

    final IObservation<TupleResult> observation = valueData.getObservation();
    if( observation == null )
      return;

    /* recreate hover info on every paint */
    clearInfoIndex();

    final TupleResult result = observation.getResult();

    for( int i = 0; i < result.size(); i++ )
    {
      final IRecord record = result.get( i );

      final Rectangle rect = getScreenRect( record );
      if( rect != null )
      {
        // new MarkerFigure( );
        final IPointStyle style = getPointStyle();
        style.setWidth( rect.width );
        style.setHeight( rect.height );

        final PointFigure ps = new PointFigure( style );
        ps.setCenterPoint( RectangleUtils.getCenterPoint( rect ) );
        ps.paint( gc );

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
    final Double dN = ProfileUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_ROHR_DN, record );
    final Double sT = ProfileUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION, record );
    final Double uK = ProfileUtil.getDoubleValueFor( IWspmConstants.LENGTH_SECTION_PROPERTY_GROUND, record );
    if( uK.isNaN() || dN.isNaN() || sT.isNaN() )
      return null;

    final ICoordinateMapper<Double, Double> coordinateMapper = getCoordinateMapper();

    final Point pOK = coordinateMapper.numericToScreen( sT, uK );
    final Point pBase = coordinateMapper.numericToScreen( 0.0, 0.0 );
    final Point pDN = coordinateMapper.numericToScreen( dN / 1000, dN );
    final int dx = pBase.x - pDN.x;
    final int dy = pBase.y - pDN.y;
    return new Rectangle( pOK.x - dx / 2, pOK.y - dy, dx, dy );
  }
}
package org.kalypso.model.wspm.ui.profil.view.chart.layer;

import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfil.PROFIL_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.profil.view.IProfilView;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;
import org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.profil.view.panel.RauheitenPanel;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;
import de.belger.swtchart.util.LogicalRange;

/**
 * @author Gernot Belger
 */
public class RauheitLayer extends AbstractProfilChartLayer implements IProfilChartLayer
{
  private final Color m_color;

  // private final IProfil m_profil;

  private final Color m_fillColor;

  private IProfilEventManager m_pem;

  public RauheitLayer( final ProfilChartView pvp, final AxisRange domainRange, final AxisRange valueRange, final Color color, final Color fillColor )
  {
    super( pvp, domainRange, valueRange );

    m_pem = pvp.getProfilEventManager();
    m_color = color;
    m_fillColor = fillColor;

  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  public Rectangle2D getBounds( )
  {
    final List<IProfilPoint> points = m_pem.getProfil().getPoints();

    Rectangle2D bounds = null;

    try
    {
      for( final IProfilPoint p : points )
      {
        final double x = p.getValueFor( POINT_PROPERTY.BREITE );

        final double rauheit = p.getValueFor( POINT_PROPERTY.RAUHEIT );
        final Rectangle2D area = new Rectangle2D.Double( x, 0.0, 0.0, rauheit );

        if( bounds == null )
          bounds = area;
        else
          bounds.add( area );
      }
    }
    catch( final Exception e )
    {
      // should never happen
      e.printStackTrace();
    }

    return bounds;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paint( final GCWrapper gc )
  {
    final Color background = gc.getBackground();
    final IProfil profil = getProfil();
    if( profil == null )
      return;
    IProfilDevider[] deviders = profil.getDevider( new DEVIDER_TYP[] { DEVIDER_TYP.TRENNFLAECHE, DEVIDER_TYP.DURCHSTROEMTE } );
    List<IProfilPoint> points = new ArrayList<IProfilPoint>();
    double[] values;
    if( getViewData().useDeviderValue() && deviders.length == 4 )
    {
      values = new double[4];
      int i = 0;
      for( IProfilDevider dev : deviders )
      {
        final Double value = (Double)dev.getValueFor( IProfilPoint.POINT_PROPERTY.RAUHEIT );
        try
        {
          values[i++] = value == null ? dev.getPoint().getValueFor(IProfilPoint.POINT_PROPERTY.RAUHEIT ) :value;
        }
        catch( ProfilDataException e )
        {
          values[i++] =0.0;
        }
        points.add( dev.getPoint() );
      }
    }
    else
    {
      points = getProfil().getPoints();
      try
      {
        values = getProfil().getValuesFor(POINT_PROPERTY.RAUHEIT);
      }
      catch( ProfilDataException e )
      {
        values = new double[points.size()];
      }
    }
   
    IProfilPoint lastP = null;
    int i = 0;
    for( final Iterator<IProfilPoint> pIt = points.iterator(); pIt.hasNext(); )
    {
      final IProfilPoint p = pIt.next();

      if( lastP != null )
      {
        try
        {
          final double x1 = lastP.getValueFor( POINT_PROPERTY.BREITE );
          final double x2 = p.getValueFor( POINT_PROPERTY.BREITE );
          //final double y1 = 0;
          //final double y2 = lastP.getValueFor( POINT_PROPERTY.RAUHEIT );
          final Rectangle box = logical2screen( new Rectangle2D.Double( x1, 0.0, x2 - x1, values[i++] ) );
          box.width += 1;
          fillRectangle( gc, box );
        }
        catch( final ProfilDataException e )
        {
          // sollte nie passieren
        }
      }
      lastP = p;

    }
    gc.setBackground( background );
  }

  private void fillRectangle( final GCWrapper gc, final Rectangle box )
  {
    // gc.setAlpha( 50 );
    gc.setForeground( m_color );
    gc.setBackground( m_fillColor );
    gc.fillRectangle( box );
    // gc.drawRectangle( box );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHoverInfo( final Point point )
  {
    return null;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintDrag(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  @Override
  public void paintDrag( final GCWrapper gc, final Point editing, final Object hoverData )
  {
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final Object rw = m_pem.getProfil().getProperty( PROFIL_PROPERTY.RAUHEIT_TYP );
    return "Rauheit " + ((rw == null) ? "unbekannter Typ" : rw.toString());
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintLegend(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public void paintLegend( final GCWrapper gc )
  {
    final Rectangle clipping = gc.getClipping();
    fillRectangle( gc, clipping );
  }

  @Override
  public IProfilView createLayerPanel( final IProfilEventManager pem, final ProfilViewData viewData )
  {
    return new RauheitenPanel( pem, viewData );
  }

  /**
   * @see IProfilChartLayer#removeYourself()
   */
  public void removeYourself( )
  {
    final IProfilChange change = new PointPropertyRemove( m_pem.getProfil(), POINT_PROPERTY.RAUHEIT );

    final ProfilOperation operation = new ProfilOperation( "Datensatz entfernen: " + toString(), m_pem, change, true );
    new ProfilOperationJob( operation ).schedule();
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.AbstractProfilChartLayer#editProfil(org.eclipse.swt.graphics.Point,
   *      java.lang.Object)
   */
  @Override
  protected void editProfil( Point point, Object data )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
    if( !hint.isPointValuesChanged() )
      return;
    final AxisRange valueRange = getValueRange();
    final double max = valueRange.getLogicalTo();
    double maxProfilValue = 0;
    for( IProfilChange change : changes )
    {
      if( change.getPointProperty() == POINT_PROPERTY.RAUHEIT )
      {
        final double newValue = change.getValue();
        if( newValue > maxProfilValue )
          maxProfilValue = newValue;
      }
    }
    final double newMax = 1.05 * maxProfilValue;
    if( Math.abs( maxProfilValue - max ) > max * 0.05 )
      valueRange.setLogicalRange( new LogicalRange( valueRange.getLogicalFrom(), newMax ) );

  }
}

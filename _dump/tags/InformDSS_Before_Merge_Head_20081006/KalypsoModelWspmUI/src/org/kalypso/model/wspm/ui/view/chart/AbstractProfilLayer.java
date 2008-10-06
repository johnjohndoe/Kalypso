/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.ui.view.chart;

import java.awt.geom.Point2D;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

import de.openali.odysseus.chart.ext.base.layer.AbstractChartLayer;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.data.impl.DataRange;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.impl.LineStyle;
import de.openali.odysseus.chart.framework.model.style.impl.PointStyle;

/**
 * @author kimwerner
 */
public abstract class AbstractProfilLayer extends AbstractChartLayer implements IProfilChartLayer
{

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#setTargetComponent(java.lang.String)
   */
  public void setTargetComponent( String componentId )
  {
    m_targetComponent = componentId;

  }

  private IProfil m_profil;

  private String m_targetComponent;

  private final String m_domainComponent;

  private ILineStyle m_LineStyle = null;

  private ILineStyle m_LineStyle_active = null;

  private ILineStyle m_LineStyle_hover = null;

  private IPointStyle m_pointStyle = null;

  private IPointStyle m_pointStyle_active = null;

  private IPointStyle m_pointStyle_hover = null;

  private boolean m_isLocked = false;

  protected static String TOOLTIP_FORMAT = "%-12s %10.4f [m]%n%-12s %10.4f [%s]";

  public AbstractProfilLayer( final IProfil profil, final String targetRangeProperty, final ILayerStyleProvider styleProvider )
  {
    m_profil = profil;
    m_domainComponent = IWspmConstants.POINT_PROPERTY_BREITE;
    m_targetComponent = targetRangeProperty;
    if( styleProvider != null )
    {
      final String id = getId();
      setLineStyle( styleProvider.getStyleFor( id + "_LINE", LineStyle.class ) );
      setLineStyle_active( styleProvider.getStyleFor( id + "_LINE_ACTIVE", LineStyle.class ) );
      setLineStyle_hover( styleProvider.getStyleFor( id + "_LINE_HOVER", LineStyle.class ) );
      setPointStyle( styleProvider.getStyleFor( id + "_POINT", PointStyle.class ) );
      setPointStyle_active( styleProvider.getStyleFor( id + "_POINT_ACTIVE", PointStyle.class ) );
      setPointStyle_hover( styleProvider.getStyleFor( id + "_POINT_HOVER", PointStyle.class ) );
    }
  }

  public void setLineStyle( ILineStyle lineStyle )
  {
    m_LineStyle = lineStyle;
  }

  public void setLineStyle_active( ILineStyle lineStyle_active )
  {
    m_LineStyle_active = lineStyle_active;
  }

  public void setLineStyle_hover( ILineStyle lineStyle_hover )
  {
    m_LineStyle_hover = lineStyle_hover;
  }

  public void setPointStyle( IPointStyle pointStyle )
  {
    m_pointStyle = pointStyle;
  }

  public void setPointStyle_active( IPointStyle pointStyle_active )
  {
    m_pointStyle_active = pointStyle_active;
  }

  public void setPointStyle_hover( IPointStyle pointStyle_hover )
  {
    m_pointStyle_hover = pointStyle_hover;
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#commitDrag(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  public EditInfo commitDrag( Point point, EditInfo dragStartData )
  {

    if( getTargetComponent() != null )
      getProfil().setActivePointProperty( getTargetComponent() );

    if( dragStartData.m_pos == point )
      executeClick( dragStartData );
    else
      executeDrop( point, dragStartData );

    return null;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#createLayerPanel()
   */
  public IProfilView createLayerPanel( )
  {
    // override this method
    return null;
  }

  /**
   * @see de.openali.odysseus.chart.ext.base.layer.AbstractChartLayer#createLegendEntries()
   */
  @Override
  protected ILegendEntry[] createLegendEntries( )
  {
    // override this method
    return null;// new ILegendEntry[] {};
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#dispose()
   */
  public void dispose( )
  {
    /**
     * don't dispose Styles, StyleProvider will do
     */

  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#drag(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */

  public EditInfo drag( Point newPos, EditInfo dragStartData )
  {
    // override this method
    return new EditInfo( this, null, null, dragStartData.m_data, "", newPos );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractProfilLayer#executeClick(de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  public void executeClick( EditInfo clickInfo )
  {
    final int pos = ((Integer) clickInfo.m_data);
    final IProfil profil = getProfil();
    profil.setActivePoint( profil.getPoint( pos ) );
    profil.setActivePointProperty( getTargetComponent() );
  }

  /**
   * To be implemented by subclasses - if needed
   * 
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#executeDrop(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  public void executeDrop( Point point, EditInfo dragStartData )
  {
  }

  public IComponent getDomainComponent( )
  {
    return getProfil() == null ? null : getProfil().hasPointProperty( m_domainComponent );
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#getDomainRange()
   */
  public IDataRange<Number> getDomainRange( )
  {

    final Double max = ProfilUtil.getMaxValueFor( getProfil(), getDomainComponent() );
    final Double min = ProfilUtil.getMinValueFor( getProfil(), getDomainComponent() );
    if( (min == null) || (max == null) )
      return null;
    return new DataRange<Number>( min, max );
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#getHover(org.eclipse.swt.graphics.Point)
   */
  public EditInfo getHover( Point pos )
  {
    if( getProfil() == null )
      return null;
    final IRecord[] profilPoints = getProfil().getPoints();
    final int len = profilPoints.length;
    for( int i = 0; i < len; i++ )
    {

      final Rectangle hover = getHoverRect( profilPoints[i] );
      if( hover == null )
        continue;
      if( hover.contains( pos ) )
        return new EditInfo( this, null, null, i, getTooltipInfo( profilPoints[i] ), pos );
    }
    return null;
  }

  @SuppressWarnings("unused")
  public Rectangle getHoverRect( final IRecord profilPoint )
  {
    return null;
  }

  /**
   * @see de.openali.odysseus.chart.ext.base.layer.AbstractChartLayer#getId()
   */
  @Override
  public String getId( )
  {
    final IComponent target = getTargetComponent();
    return target == null ? m_targetComponent : target.getId();
  }

  protected ILineStyle getLineStyle( )
  {
    return m_LineStyle;
  }

  protected ILineStyle getLineStyle_active( )
  {
    return m_LineStyle_active;
  }

  protected ILineStyle getLineStyle_hover( )
  {
    return m_LineStyle_hover;
  }

  public Point2D getPoint2D( final IRecord point )
  {
    final Double x = ProfilUtil.getDoubleValueFor( m_domainComponent, point );
    final Double y = ProfilUtil.getDoubleValueFor( m_targetComponent, point );
    return new Point2D.Double( x, y );
  }

  protected IPointStyle getPointStyle( )
  {
    return m_pointStyle;
  }

  protected IPointStyle getPointStyle_active( )
  {
    return m_pointStyle_active;
  }

  protected IPointStyle getPointStyle_hover( )
  {
    return m_pointStyle_hover;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#getProfil()
   */
  public IProfil getProfil( )
  {
    return m_profil;
  }

  public IComponent getTargetComponent( )
  {
    return getProfil() == null ? null : getProfil().hasPointProperty( m_targetComponent );
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#getTargetRange()
   */
  public IDataRange<Number> getTargetRange( )
  {

    final Double max = ProfilUtil.getMaxValueFor( getProfil(), getTargetComponent() );
    final Double min = ProfilUtil.getMinValueFor( getProfil(), getTargetComponent() );
    if( (min == null) || (max == null) )
      return null;
    return new DataRange<Number>( min, max );
  }

  /**
   * @see de.openali.odysseus.chart.ext.base.layer.AbstractChartLayer#getTitle()
   */
  @Override
  public String getTitle( )
  {
    final IComponent cmp = m_profil == null ? null : m_profil.hasPointProperty( m_targetComponent );
    return cmp == null ? m_targetComponent : cmp.getName();
  }

  public String getTooltipInfo( final IRecord point )
  {

    if( point == null || (getTargetComponent() == null) || (getDomainComponent() == null) )
      return "";
    try
    {
      final Point2D p = getPoint2D( point );
      return String.format( TOOLTIP_FORMAT, new Object[] { getDomainComponent().getName(), p.getX(), getTargetComponent().getName(), p.getY(), getTargetComponent().getUnit() } );
    }
    catch( RuntimeException e )
    {
      return e.getLocalizedMessage();
    }

  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#isLocked()
   */
  public boolean isLocked( )
  {
    return m_isLocked;
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#lockLayer(boolean)
   */
  public void lockLayer( boolean isLocked )
  {
    m_isLocked = isLocked;

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
    final IProfil profil = getProfil();
    if( profil == null )
      return;
    if( hint.isActivePointChanged() )
    {
      getEventHandler().fireLayerContentChanged( this );
    }
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#paint(org.eclipse.swt.graphics.GC)
   */
  public void paint( GC gc )
  {
    // override this method
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#removeYourself()
   */
  public void removeYourself( )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#setProfil(org.kalypso.model.wspm.core.profil.IProfil)
   */
  public void setProfil( IProfil profil )
  {
    m_profil = profil;
  }

  public Point2D toNumeric( final Point point )
  {
    final ICoordinateMapper cm = getCoordinateMapper();
    final Double x = cm.getDomainAxis().screenToNumeric( point.x ).doubleValue();
    final Double y = cm.getTargetAxis().screenToNumeric( point.y ).doubleValue();
    return new Point2D.Double( x, y );
  }

  public Point toScreen( final IRecord point )
  {
    final ICoordinateMapper cm = getCoordinateMapper();
    final Double x = ProfilUtil.getDoubleValueFor( m_domainComponent, point );
    final Double y = ProfilUtil.getDoubleValueFor( m_targetComponent, point );
    if( cm != null && x != null && y != null && !x.isNaN() && !y.isNaN() )
      return cm == null ? null : cm.numericToScreen( x, y );
    return null;
  }

}

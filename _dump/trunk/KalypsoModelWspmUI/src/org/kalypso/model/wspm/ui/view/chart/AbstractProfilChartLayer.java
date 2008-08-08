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
import java.awt.geom.Rectangle2D;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;

import de.openali.odysseus.chart.ext.base.layer.AbstractChartLayer;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;

public abstract class AbstractProfilChartLayer extends AbstractChartLayer implements IProfilChartLayer
{

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#alwaysAllowsEditing()
   */
  public boolean alwaysAllowsEditing( )
  {
    return false;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getZOrder()
   */
  public int getZOrder( )
  {
    // the layerprovider order the layers
    return 0;
  }

  private final ProfilChartView m_chartView;

  private boolean m_initialVisibility = true;

  private final String m_label;

  private final String m_id;

  protected final IAxis m_domainRange;

  protected final IAxis m_valueRange;

  public AbstractProfilChartLayer( final String layerId, final ProfilChartView chartView, final IAxis domainRange, final IAxis valueRange, final String label )
  {
    super();
    setCoordinateMapper( chartView.getMapper() );
    m_domainRange = domainRange;
    m_valueRange = valueRange;
    m_label = label;
    m_chartView = chartView;
    m_id = layerId;
  }

  public AbstractProfilChartLayer( final String layerId, final ProfilChartView chartView, final IAxis domainRange, final IAxis valueRange, final String label, final boolean initalVisibility )
  {
    super();
    setCoordinateMapper( chartView.getMapper() );
    m_domainRange = domainRange;
    m_valueRange = valueRange;
    m_label = label;
    m_chartView = chartView;
    m_initialVisibility = initalVisibility;
    m_id = layerId;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getInitialVisibility()
   */
  public boolean getInitialVisibility( )
  {
    return m_initialVisibility;
  }

  public final void edit( final Point point, final Object data )
  {
    editProfil( point, data );

  }

  /**
   * @return Returns the profil.
   */
  public final IProfil getProfil( )
  {
    return m_chartView.getProfil();
  }

  /**
   * @deprecated
   */

  protected abstract void editProfil( Point point, Object data );

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.IProfilChartLayer#createLayerPanel(org.kalypso.model.wspm.core.profil.IProfilEventManager,
   *      org.kalypso.model.wspm.ui.profil.view.ProfilViewData)
   */
  public IProfilView createLayerPanel( IProfil profile )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  public EditInfo getHoverInfo( Point point )
  {
    return null;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintDrag(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  public void paintDrag( GC gc, Point editing, Object hoverData )
  {
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintLegend(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paintLegend( GC gc )
  {
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#setActivePoint(java.lang.Object)
   */
  public void setActivePoint( Object data )
  {
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.IProfilChartLayer#getProfilChartView()
   */
  public ProfilChartView getProfilChartView( )
  {
    return m_chartView;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#getId()
   */
  public String getId( )
  {

    return m_id;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#getLabel()
   */
  public String getLabel( )
  {

    return m_label;
  }

  public final Point logical2screen( final Point2D p2d )
  {
    return new Point( m_domainRange.numericToScreen( p2d.getX() ), m_valueRange.numericToScreen( p2d.getY() ) );
  }

  public final Point2D screen2logical( final Point p )
  {
    return new Point2D.Double( (Double) m_domainRange.screenToNumeric( p.x ), (Double) m_valueRange.screenToNumeric( p.y ) );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#logical2screen(java.awt.geom.Rectangle2D)
   */
  public final Rectangle logical2screen( final Rectangle2D r2d )
  {
    final double x1 = m_domainRange.numericToScreen( r2d.getX() );
    final double y1 = m_valueRange.numericToScreen( r2d.getY() );
    final double x2 = m_domainRange.numericToScreen( r2d.getX() + r2d.getWidth() );
    final double y2 = m_valueRange.numericToScreen( r2d.getY() + r2d.getHeight() );

    return new Rectangle( (int) x1, (int) y1, (int) (x2 - x1), (int) (y2 - y1) );
  }

  /**
   * @see de.openali.odysseus.chart.ext.base.layer.AbstractChartLayer#createLegendEntries()
   */
  @Override
  protected ILegendEntry[] createLegendEntries( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#getDomainRange()
   */
  public IDataRange<Number> getDomainRange( )
  {
    return m_domainRange.getNumericRange();
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#getTargetRange()
   */
  public IDataRange<Number> getTargetRange( )
  {
    return m_valueRange.getNumericRange();
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#dispose()
   */
  public void dispose( )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#paint(org.eclipse.swt.graphics.GC)
   */
  public void paint( GC gc )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#removeYourself()
   */
  public void removeYourself( )
  {
    // TODO Auto-generated method stub

  }

}

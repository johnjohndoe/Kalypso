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
package org.kalypso.model.wspm.tuhh.ui.chart;

import java.util.ArrayList;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.observation.result.IComponent;

import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.data.impl.DataRange;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.IExpandableChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.layer.impl.LegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;

/**
 * @author kimwerner
 */
public abstract class AbstractProfilTheme extends AbstractProfilLayer implements IExpandableChartLayer
{

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getId()
   */
  @Override
  public String getId( )
  {
    return super.getId();
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IExpandableChartLayer#addLayer(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
   */
  public void addLayer( IChartLayer layer )
  {
    throw new IllegalStateException( "no layer addable" );

  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IExpandableChartLayer#getLayer()
   */
  public IChartLayer[] getLayers( )
  {
    final ArrayList<IProfilChartLayer> layers = new ArrayList<IProfilChartLayer>( m_chartLayers.length );
    for( final IProfilChartLayer layer : m_chartLayers )
      if( layer.getTargetComponent() != null )
        layers.add( layer );
    return layers.toArray( new IProfilChartLayer[] {} );
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IExpandableChartLayer#removeLayer(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
   */
  public IChartLayer removeLayer( IChartLayer layer )
  {
    throw new IllegalStateException( "no removable layer" );
  }

  protected final IProfilChartLayer[] m_chartLayers;

  private IProfilChartLayer m_hovering = null;

  private final String m_title;

  public AbstractProfilTheme( final String id, final String title, final IProfilChartLayer[] chartLayers, final ICoordinateMapper cm )
  {
    super( null, id, null );
    m_title = title;
    setCoordinateMapper( cm );
    for( final IChartLayer layer : chartLayers )
      layer.setCoordinateMapper( cm );
    m_chartLayers = chartLayers;

  }

  protected final void drawClippingRect( final GC gc )
  {
    final Color col = new Color( gc.getDevice(), new RGB( 0, 0, 0 ) );
    try
    {
      gc.setForeground( col );
      Rectangle clipping = gc.getClipping();
      gc.setLineWidth( 1 );
      gc.drawRectangle( clipping.x, clipping.y, clipping.width - 1, clipping.height - 1 );
      gc.setClipping( clipping.x + 1, clipping.y + 1, clipping.width - 2, clipping.height - 2 );
    }
    finally
    {
      col.dispose();
    }
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#drag(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  @Override
  public EditInfo drag( Point newPos, EditInfo dragStartData )
  {
    if( m_hovering != null )
      return m_hovering.drag( newPos, dragStartData );
    return null;
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#commitDrag(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  @Override
  public EditInfo commitDrag( Point point, EditInfo dragStartData )
  {

    if( getTargetComponent() != null )
      getProfil().setActivePointProperty( getTargetComponent() );

    if( dragStartData.m_pos == point )
      m_hovering.executeClick( dragStartData );
    else
      m_hovering.executeDrop( point, dragStartData );

    return null;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#executeClick(de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */

  @Override
  public void executeClick( EditInfo clickInfo )
  {
    if( m_hovering != null )
      m_hovering.executeClick( clickInfo );
    m_hovering = null;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractProfilLayer#executeDrop(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */

  @Override
  public void executeDrop( Point point, EditInfo dragStartData )
  {
    if( m_hovering != null )
      m_hovering.executeDrop( point, dragStartData );
    m_hovering = null;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getDomainComponent()
   */

  @Override
  public IComponent getDomainComponent( )
  {
    return m_hovering == null ? null : m_hovering.getDomainComponent();
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getHover(org.eclipse.swt.graphics.Point)
   */

  @Override
  public EditInfo getHover( Point pos )
  {
    for( int i = m_chartLayers.length - 1; i > -1; i-- ) // reverse layers, last paint-hover first
    {
      final EditInfo info = m_chartLayers[i].getHover( pos );
      if( info != null )
      {
        m_hovering = m_chartLayers[i];
        return info;
      }
    }
    return null;

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getTargetComponent()
   */

  @Override
  public IComponent getTargetComponent( )
  {
    return m_hovering == null ? null : m_hovering.getTargetComponent();

  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#paint(org.eclipse.swt.graphics.GC)
   */
  @Override
  public void paint( final GC gc )
  {
    for( final IProfilChartLayer layer : m_chartLayers )
      if( layer.isVisible() )
        layer.paint( gc );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#removeYourself()
   */
  @Override
  public void removeYourself( )
  {
    for( final IProfilChartLayer layer : m_chartLayers )
      layer.removeYourself();

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getDomainRange()
   */
  @Override
  public IDataRange<Number> getDomainRange( )
  {
    Double min = null;
    Double max = null;
    for( final IChartLayer layer : m_chartLayers )
    {

      final IDataRange<Number> dr = layer.getDomainRange();
      if( dr != null )
      {
        if( max == null )
          max = dr.getMax().doubleValue();
        else
          max = Math.max( max, dr.getMax().doubleValue() );
        if( min == null )
          min = dr.getMin().doubleValue();
        else
          min = Math.min( min, dr.getMin().doubleValue() );
      }
    }
    if( (min == null) || (max == null) )
      return null;
    return new DataRange<Number>( min, max );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getTargetRange()
   */
  @Override
  public IDataRange<Number> getTargetRange( )
  {
    Double min = null;
    Double max = null;
    for( final IChartLayer layer : m_chartLayers )
    {
      final IDataRange<Number> dr = layer.getTargetRange();
      if( dr != null )
      {
        if( max == null )
          max = dr.getMax().doubleValue();
        else
          max = Math.max( max, dr.getMax().doubleValue() );
        if( min == null )
          min = dr.getMin().doubleValue();
        else
          min = Math.min( min, dr.getMin().doubleValue() );
      }
    }
    if( (min == null) || (max == null) )
      return null;
    return new DataRange<Number>( min, max );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getProfil()
   */
  @Override
  public IProfil getProfil( )
  {
    if( (m_chartLayers.length > 0) && (m_chartLayers[0] != null) )
      return m_chartLayers[0].getProfil();
    return null;
  }

  /**
   * @see de.openali.odysseus.chart.ext.base.layer.AbstractChartLayer#getLegendEntries()
   */
  @Override
  public ILegendEntry[] createLegendEntries( )
  {
    LegendEntry le = new LegendEntry( this, toString() )
    {
      @Override
      public void paintSymbol( GC gc, Point size )
      {
        drawClippingRect( gc );
      }
    };
    return new ILegendEntry[] { le };
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getTitle()
   */
  @Override
  public String getTitle( )
  {
    return m_title;
  }

  public final boolean moveLayerToPosition( final String id, final IChartLayer selectedObject )
  {

    IProfilChartLayer draggedLayer = null;
    int source = -1;
    int target = -1;
    for( int i = 0; i < m_chartLayers.length; i++ )
    {

      if( m_chartLayers[i].getId().equals( id ) )
      {
        draggedLayer = m_chartLayers[i];
        source = i;
      }
      if( m_chartLayers[i] == selectedObject )
        target = i;
    }
    if( target == source )
      return false;
    else if( target - source > 0 )
      for( int i = source; i < target; i++ )
        m_chartLayers[i] = m_chartLayers[i + 1];
    else if( target - source < 0 )
      for( int i = source; i > target; i-- )
        m_chartLayers[i] = m_chartLayers[i - 1];
    m_chartLayers[target] = draggedLayer;
    fireLayerContentChanged();
    return true;
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IExpandableChartLayer#fireLayerChanged(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
   */
  public void fireLayerContentChanged( )
  {
    getEventHandler().fireLayerContentChanged( this );
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IExpandableChartLayer#fireLayerVisibilityChanged()
   */
  public void fireLayerVisibilityChanged( )
  {
    fireLayerContentChanged();
  }
}

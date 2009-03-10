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

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.i18n.Messages;
import org.kalypso.observation.result.IComponent;

import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.data.impl.DataRange;
import de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener;
import de.openali.odysseus.chart.framework.model.event.impl.AbstractLayerManagerEventListener;
import de.openali.odysseus.chart.framework.model.figure.impl.EmptyRectangleFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer;
import de.openali.odysseus.chart.framework.model.layer.IExpandableChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.layer.impl.LayerManager;
import de.openali.odysseus.chart.framework.model.layer.impl.LegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.util.StyleUtils;

/**
 * @author kimwerner
 */
public abstract class AbstractProfilTheme extends AbstractProfilLayer implements IExpandableChartLayer
{

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#setProfil(org.kalypso.model.wspm.core.profil.IProfil)
   */
  @Override
  public void setProfil( IProfil profil )
  {
    super.setProfil( profil );
    for (final IChartLayer layer : getLayerManager().getLayers())
    {
      if (layer instanceof IProfilChartLayer)
        ((IProfilChartLayer)layer).setProfil( profil );
       
    }
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
    if( hint.isActivePointChanged() )
    {
      fireLayerContentChanged();
    }
    else
      for( final IChartLayer layer : getLayerManager().getLayers() )
      {
        if( layer instanceof IProfilChartLayer )
          ((IProfilChartLayer) layer).onProfilChanged( hint, changes );
      }
  }

  private final ILayerManagerEventListener m_eventListener = new AbstractLayerManagerEventListener()
  {

    /**
     * @see de.openali.odysseus.chart.framework.model.event.impl.AbstractLayerManagerEventListener#onLayerContentChanged(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
     */
    @Override
    public void onLayerContentChanged( IChartLayer layer )
    {
      fireLayerContentChanged();
    }

    /**
     * @see de.openali.odysseus.chart.framework.model.event.impl.AbstractLayerManagerEventListener#onLayerVisibilityChanged(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
     */
    @Override
    public void onLayerVisibilityChanged( IChartLayer layer )
    {
      fireLayerContentChanged();
    }

    /**
     * @see de.openali.odysseus.chart.framework.model.event.impl.AbstractLayerManagerEventListener#onLayerMoved(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
     */
    @Override
    public void onLayerMoved( IChartLayer layer )
    {
      fireLayerContentChanged();
    }

    /**
     * @see de.openali.odysseus.chart.framework.model.event.impl.AbstractLayerManagerEventListener#onActivLayerChanged(de.openali.odysseus.chart.framework.model.layer.IChartLayer)
     */
    @Override
    public void onActivLayerChanged( IChartLayer layer )
    {
      // if layer is deactivated do nothing
      if( !layer.isActive() )
        return;
      if( !isActive() )
        setActive( true );
      for( final IChartLayer l : getLayerManager().getLayers() )
      {
        if( l != layer )
          l.setActive( false );
      }

    }
  };

  private final String m_id;

  private final ILayerManager m_layerManager = new LayerManager();

  private final String m_title;

  public AbstractProfilTheme(final IProfil profil, final String id, final String title, final IProfilChartLayer[] chartLayers, final ICoordinateMapper cm )
  {
    super( profil, id, null );
    m_title = title;
    m_id = id;
    setCoordinateMapper( cm );
    m_layerManager.addListener( m_eventListener );
    if( chartLayers != null )
      for( final IChartLayer layer : chartLayers )
      {
        m_layerManager.addLayer( layer );
        layer.setCoordinateMapper( cm );
      }
  }

  protected final void fireLayerContentChanged( )
  {
    getEventHandler().fireLayerContentChanged( this );
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
    final IProfilChartLayer layer = getActiveLayer();
    if( layer == null )
      return null;

    if( dragStartData.m_pos == point )
      layer.executeClick( dragStartData );
    else
      layer.executeDrop( point, dragStartData );

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
        for( final IChartLayer layer : getLayerManager().getLayers() )
        {
          final ILegendEntry[] les = layer.getLegendEntries();
          if( les == null )
            continue;
          for( final ILegendEntry l : les )
          {
            ((LegendEntry) l).paintSymbol( gc, size );
          }
        }
      }
    };
    return new ILegendEntry[] { le };
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#drag(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  @Override
  public EditInfo drag( Point newPos, EditInfo dragStartData )
  {
    final IProfilChartLayer layer = getActiveLayer();
    if( layer == null || layer.isLocked() )
      return null;
    return layer.drag( newPos, dragStartData );
  }

  private final IProfilChartLayer getActiveLayer( )
  {
    for( final IChartLayer l : getLayerManager().getLayers() )
    {
      if( l.isActive() && l instanceof IProfilChartLayer )
        return (IProfilChartLayer) l;
    }
    return null;
  }

  public final void drawClippingRect( final GC gc )
  {
    final EmptyRectangleFigure rf = new EmptyRectangleFigure();
    rf.setStyle( StyleUtils.getDefaultStyle( ILineStyle.class ) );
    Rectangle clipping = gc.getClipping();
    rf.setRectangle( clipping );
    rf.paint( gc );
    gc.setClipping( clipping.x + 2, clipping.y + 2, clipping.width - 4, clipping.height - 4 );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#executeClick(de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */

  @Override
  public void executeClick( EditInfo clickInfo )
  {
    final IProfilChartLayer layer = getActiveLayer();
    if( layer != null )
      layer.executeClick( clickInfo );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractProfilLayer#executeDrop(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */

  @Override
  public void executeDrop( Point point, EditInfo dragStartData )
  {
    final IProfilChartLayer layer = getActiveLayer();
    if( layer != null )
      layer.executeDrop( point, dragStartData );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getDomainComponent()
   */

  @Override
  public IComponent getDomainComponent( )
  {
    final IProfilChartLayer layer = getActiveLayer();
    return layer == null ? null : layer.getDomainComponent();
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getDomainRange()
   */
  @Override
  public IDataRange<Number> getDomainRange( )
  {
    Double min = null;
    Double max = null;
    for( final IChartLayer layer : getLayerManager().getLayers() )
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
    if( min == null || max == null )
      return null;
    return new DataRange<Number>( min, max );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getHover(org.eclipse.swt.graphics.Point)
   */

  @Override
  public EditInfo getHover( Point pos )
  {
    final IChartLayer[] layers = getLayerManager().getLayers();
    for( int i = layers.length - 1; i > -1; i-- ) // reverse layers, last paint will hover first
    {
      if( layers[i] instanceof IProfilChartLayer )
      {
        final IProfilChartLayer pLayer = (IProfilChartLayer) layers[i];
        final EditInfo info = pLayer.getHover( pos );
        if( info != null )
        {
          if( !pLayer.isActive() )
            pLayer.setActive( true );
          return info;
        }
      }
    }
    return null;

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getId()
   */
  @Override
  public String getId( )
  {
    return m_id;
  }

  public final ILayerManager getLayerManager( )
  {
    return m_layerManager;
  }

//  /**
//   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getProfil()
//   */
//  @Override
//  public IProfil getProfil( )
//  {
//    for( final IChartLayer layer : getLayerManager().getLayers() )
//    {
//      if( layer instanceof IProfilChartLayer )
//      {
//        final IProfilChartLayer pLayer = (IProfilChartLayer) layer;
//        final IProfil profil = pLayer.getProfil();
//        if( profil != null )
//          return profil;
//      }
//    }
//    return null;
//  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getTargetComponent()
   */

  @Override
  public IComponent getTargetComponent( )
  {
    final IProfilChartLayer layer = getActiveLayer();
    return layer == null ? null : layer.getTargetComponent();

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getTargetRange()
   */
  @Override
  public IDataRange<Number> getTargetRange( )
  {
    Double min = null;
    Double max = null;
    for( final IChartLayer layer : getLayerManager().getLayers() )
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
    if( min == null || max == null )
      return null;
    return new DataRange<Number>( min, max );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getTitle()
   */
  @Override
  public String getTitle( )
  {
    return m_title;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#lockLayer(boolean)
   */
  @Override
  public void lockLayer( boolean isLocked )
  {
    if( isLocked != isLocked() )
      for( final IEditableChartLayer layer : getLayerManager().getEditableLayers() )
      {
        layer.lockLayer( isLocked );
      }
    super.lockLayer( isLocked );

  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#paint(org.eclipse.swt.graphics.GC)
   */
  @Override
  public void paint( final GC gc )
  {
    for( final IChartLayer layer : getLayerManager().getLayers() )
      if( layer.isVisible() )
        layer.paint( gc );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#removeYourself()
   */
  @Override
  public void removeYourself( )
  {
    throw new UnsupportedOperationException( Messages.getString("org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme.0") ); //$NON-NLS-1$
  }
}

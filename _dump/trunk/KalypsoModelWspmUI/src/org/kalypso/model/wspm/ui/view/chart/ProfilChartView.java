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

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPersistableElement;
import org.kalypso.chart.ui.IChartPart;
import org.kalypso.chart.ui.editor.commandhandler.ChartHandlerUtilities;
import org.kalypso.chart.ui.editor.mousehandler.AxisDragHandlerDelegate;
import org.kalypso.chart.ui.editor.mousehandler.PlotDragHandlerDelegate;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.result.IStationResult;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIExtensions;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.view.chart.color.IProfilColorSet;
import org.kalypso.model.wspm.ui.view.chart.handler.ClickHandler;

import de.openali.odysseus.chart.ext.base.axis.GenericLinearAxis;
import de.openali.odysseus.chart.ext.base.axisrenderer.AxisRendererConfig;
import de.openali.odysseus.chart.ext.base.axisrenderer.GenericAxisRenderer;
import de.openali.odysseus.chart.ext.base.axisrenderer.GenericNumberTickCalculator;
import de.openali.odysseus.chart.ext.base.axisrenderer.NumberLabelCreator;
import de.openali.odysseus.chart.framework.model.impl.ChartModel;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.IExpandableChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.mapper.IAxisConstants.POSITION;
import de.openali.odysseus.chart.framework.model.mapper.impl.AxisAdjustment;
import de.openali.odysseus.chart.framework.model.mapper.registry.IMapperRegistry;
import de.openali.odysseus.chart.framework.model.mapper.renderer.IAxisRenderer;
import de.openali.odysseus.chart.framework.util.ChartUtilities;
import de.openali.odysseus.chart.framework.view.impl.ChartComposite;

/**
 * @author belger
 * @author kimwerner
 */
public class ProfilChartView extends AbstractProfilView implements IPersistableElement
{
  private static final String MEM_LAYER_VIS = "layerVisibility"; //$NON-NLS-1$

  private static final String MEM_LAYER_ACT = "activeLayer"; //$NON-NLS-1$

  private static final String MEM_LAYER_POS = "layerPosition"; //$NON-NLS-1$

  public static final String ID_AXIS_DOMAIN = "domain";//$NON-NLS-1$

  public static final String ID_AXIS_LEFT = "left";//$NON-NLS-1$

  public static final String ID_AXIS_RIGHT = "right";//$NON-NLS-1$

  public static final int AXIS_GAP = 5; // distance between layers and Axis

  private final ColorRegistry m_colorRegistry; // defines the layers Line and PointStyle colors

  // private static final int TRASH_HOLD = 3; // mouse move pixels before start editing

  private ICoordinateMapper m_mapper;

  private ChartComposite m_chart = null;

  private IProfilLayerProvider m_layerProvider;

  private PlotDragHandlerDelegate m_plotDragHandler;

  private AxisDragHandlerDelegate m_axisDragHandler;

  private IMemento m_memento = null;

  public ProfilChartView( final IProfil profile, final ColorRegistry colorRegistry )
  {
    this( profile, null, colorRegistry );
  }

  public ProfilChartView( final IProfil profile, final IStationResult[] results, final ColorRegistry colorRegistry )
  {
    super( profile, results );
    m_colorRegistry = colorRegistry;
  }

  @SuppressWarnings("boxing")//$NON-NLS-1$
  protected void createLayer( )
  {
    final ILayerManager lm = m_chart.getChartModel().getLayerManager();

    String activeLayer = "";
    // get visibility and clear layer
    final Map<String, Boolean> visibility = new HashMap<String, Boolean>();

    for( final IChartLayer layer : lm.getLayers() )
    {
      visibility.put( layer.getId(), layer.isVisible() );
      if( layer.isActive() )
        activeLayer = layer.getId();
      lm.removeLayer( layer );
    }

    final IProfil profil = getProfil();
    if( profil != null )
    {
      if( m_layerProvider == null )
      {
        m_layerProvider = KalypsoModelWspmUIExtensions.createProfilLayerProvider( profil.getType() );

        if( m_layerProvider == null )
          // TODO: display userinformation
          return;
      }
      // call provider
      final String[] requieredLayer = m_layerProvider.getRequiredLayer( this );
      if( activeLayer.equals( "" ) )
        activeLayer = requieredLayer[0];
      for( final String layerId : requieredLayer )
      {
        final IProfilChartLayer layer = m_layerProvider.createLayer( layerId, this );
        if( layer != null )
        {
          final Boolean visible = visibility.get( layer.getId() );
          layer.setVisible( visible == null ? layer.isVisible() : visible );
          if( activeLayer.equals( layer.getId() ) )
            layer.setActive( true );
          lm.addLayer( layer );
        }
      }
    }

  }

  /**
   * @see com.bce.profil.ui.view.IProfilView#dispose()
   */
  @Override
  public void dispose( )
  {
    if( (m_chart != null) && !m_chart.isDisposed() )
      m_chart.dispose();
    m_chart = null;

    ChartHandlerUtilities.updateElements( this );

    super.dispose();
  }

  /**
   * @see com.bce.profil.ui.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite, int)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final int style )
  {
    m_chart = new ChartComposite( parent, style, new ChartModel(), m_colorRegistry.getRGB( IProfilColorSet.COLOUR_AXIS_BACKGROUND ) );
    m_chart.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    IMapperRegistry mr = m_chart.getChartModel().getMapperRegistry();

    final IAxis domainAxis = new GenericLinearAxis( ID_AXIS_DOMAIN, POSITION.BOTTOM, null );
    AxisAdjustment aaDom = new AxisAdjustment( 3, 94, 3 );
    domainAxis.setPreferredAdjustment( aaDom );

    final IAxis targetAxisLeft = new GenericLinearAxis( ID_AXIS_LEFT, POSITION.LEFT, null );
    AxisAdjustment aaLeft = new AxisAdjustment( 15, 75, 10 );
    targetAxisLeft.setPreferredAdjustment( aaLeft );

    final IAxis targetAxisRight = new GenericLinearAxis( ID_AXIS_RIGHT, POSITION.RIGHT, null );
    AxisAdjustment aaRight = new AxisAdjustment( 2, 40, 58 );
    targetAxisRight.setPreferredAdjustment( aaRight );

    domainAxis.setLabel( "[m]" );

    targetAxisLeft.setLabel( "[m+NN]" );
    targetAxisRight.setLabel( "[KS]" );

    mr.addMapper( domainAxis );
    mr.addMapper( targetAxisLeft );
    mr.addMapper( targetAxisRight );

    AxisRendererConfig configDom = new AxisRendererConfig();
    IAxisRenderer aRendDom = new GenericAxisRenderer( "rendDom", new NumberLabelCreator( "%s" ), new GenericNumberTickCalculator(), configDom );

    AxisRendererConfig configLR = new AxisRendererConfig();
    configLR.gap = AXIS_GAP;
    IAxisRenderer aRendLR = new GenericAxisRenderer( "rendLR", new NumberLabelCreator( "%s" ), new GenericNumberTickCalculator(), configLR );

    mr.setRenderer( ID_AXIS_DOMAIN, aRendDom );
    mr.setRenderer( ID_AXIS_LEFT, aRendLR );
    mr.setRenderer( ID_AXIS_RIGHT, aRendLR );

    try
    {
      createLayer();
    }
    catch( final Throwable e )
    {
      // catch everything, because if we get a runtime exception here
      // all goes down the privy...
      e.printStackTrace();
    }

    ChartUtilities.maximize( m_chart.getChartModel() );

    new ClickHandler( m_chart );

    m_plotDragHandler = new PlotDragHandlerDelegate( m_chart );
    m_axisDragHandler = new AxisDragHandlerDelegate( m_chart );

    return m_chart;
  }

  public final ICoordinateMapper getMapper( )
  {
    return m_mapper;
  }

  public ChartComposite getChart( )
  {
    return m_chart;
  }

  public ColorRegistry getColorRegistry( )
  {
    return m_colorRegistry;
  }

  /**
   * @deprecated use getAxis(String id)
   */
  @Deprecated
  public IAxis getDomainRange( )
  {
    return m_chart.getChartModel().getMapperRegistry().getAxis( ID_AXIS_DOMAIN );
  }

  /**
   * @see org.eclipse.ui.IPersistableElement#getFactoryId()
   */
  public String getFactoryId( )
  {
    return null;
  }

  /**
   * @deprecated use getAxis(String id)
   */
  @Deprecated
  public IAxis getValueRangeLeft( )
  {
    return m_chart.getChartModel().getMapperRegistry().getAxis( ID_AXIS_LEFT );
  }

  /**
   * @deprecated use getAxis(String id)
   */
  @Deprecated
  public IAxis getValueRangeRight( )
  {
    return m_chart.getChartModel().getMapperRegistry().getAxis( ID_AXIS_RIGHT );
  }

  public IAxis getAxis( final String id )
  {
    return m_chart.getChartModel().getMapperRegistry().getAxis( id );
  }

  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    final ChartComposite chart = m_chart;
    if( (chart == null) || chart.isDisposed() )
      return;

    chart.getDisplay().syncExec( new Runnable()
    {
      public void run( )
      {
        if( hint.isObjectChanged() || hint.isPointPropertiesChanged() )
        {
          String act = null;
          for( final IChartLayer layer : chart.getChartModel().getLayerManager().getLayers() )
          {
            if( layer.isActive() )
              act = layer.getId();
          }
          createLayer();

          for( final IChartLayer layer : chart.getChartModel().getLayerManager().getLayers() )
          {
            if( act != null )
              layer.setActive( act.equals( layer.getId() ) );
          }

          return;
        }
        else if( hint.isPointsChanged() || hint.isMarkerDataChanged() || hint.isPointValuesChanged() || hint.isObjectDataChanged() || hint.isMarkerMoved() || hint.isProfilPropertyChanged()
            || hint.isActivePointChanged() || hint.isActivePropertyChanged() )
          for( final IChartLayer layer : chart.getChartModel().getLayerManager().getLayers() )
            if( layer instanceof IProfilChartLayer )
              ((IProfilChartLayer) layer).onProfilChanged( hint, changes );
        sortLayer();
        redrawChart();
      }
    } );
  }

  protected void redrawChart( )
  {
    final ChartComposite chart = m_chart;
    if( (chart != null) && !chart.isDisposed() )
      chart.getDisplay().syncExec( new Runnable()
      {

        public void run( )
        {
          chart.redraw();
        }
      } );
  }

  public void restoreState( final IMemento memento )
  {
    if( m_chart == null )
      return;

    final Map<String, Boolean> hash = new HashMap<String, Boolean>();
    final IMemento[] layerChildren = memento.getChildren( MEM_LAYER_VIS );
    for( final IMemento layermem : layerChildren )
    {
      final String name = layermem.getID();
      final String textData = layermem.getTextData();
      final Boolean visib = Boolean.valueOf( textData );
      hash.put( name, visib );
    }

    final Map<String, Boolean> actHash = new HashMap<String, Boolean>();
    final IMemento[] actChildren = memento.getChildren( MEM_LAYER_ACT );
    for( final IMemento layermem : actChildren )
    {
      final String name = layermem.getID();
      final String textData = layermem.getTextData();
      final Boolean active = Boolean.valueOf( textData );
      actHash.put( name, active );
    }

    Boolean activeLayerFound = false;
    for( final IChartLayer layer : m_chart.getChartModel().getLayerManager().getLayers() )
    {
      final Boolean visib = hash.get( layer.getId() );
      final Boolean active = actHash.get( layer.getId() );
      if( visib != null )
        layer.setVisible( visib );
      if( active != null )
      {
        layer.setActive( active );
        activeLayerFound = activeLayerFound || active;
      }

      if( layer instanceof IExpandableChartLayer )
      {
        for( final IChartLayer child : ((IExpandableChartLayer) layer).getLayerManager().getLayers() )
        {
          final Boolean v = hash.get( child.getId() );
          if( v != null )
            child.setVisible( v );
        }
      }
    }

    m_memento = memento;
    sortLayer();

    if( !activeLayerFound )
      m_chart.getChartModel().getLayerManager().getLayers()[0].setActive( true );
  }

  protected final void sortLayer( )
  {
    if( m_memento == null )
      return;
    final Comparator<IMemento> memComp = new Comparator<IMemento>()
    {

      @Override
      public int compare( IMemento o1, IMemento o2 )
      {

        final Integer pos1 = Integer.valueOf( o1.getTextData() );
        final Integer pos2 = Integer.valueOf( o2.getTextData() );
        if( pos1 == pos2 )
          return 0;
        else if( pos1 > pos2 )
          return 1;
        else
          return -1;
      }
    };
    final IMemento[] posMem = m_memento.getChildren( MEM_LAYER_POS );
    Arrays.sort( posMem, memComp );
    int j = 0;
    for( final IMemento mem : posMem )
    {
      final IChartLayer layer = m_chart.getChartModel().getLayerManager().getLayerById( mem.getID() );
      if( layer != null )
        m_chart.getChartModel().getLayerManager().moveLayerToPosition( layer, j++ );
    }
  }

  /**
   * @see org.eclipse.ui.IPersistableElement#saveState(org.eclipse.ui.IMemento)
   */
  public void saveState( final IMemento memento )
  {
    if( m_chart == null )
      return;

    int pos = 0;
    for( final IChartLayer layer : m_chart.getChartModel().getLayerManager().getLayers() )
      if( layer != null )
      {
        final IMemento layermem = getMementoChild( memento, MEM_LAYER_VIS, layer.getId(), true );

        final IMemento layerpos = getMementoChild( memento, MEM_LAYER_POS, layer.getId(), true );
        layermem.putTextData( "" + layer.isVisible() ); //$NON-NLS-1$
        layerpos.putTextData( "" + pos++ ); //$NON-NLS-1$
        // nur eine Ebene tiefer

        if( layer instanceof IExpandableChartLayer )
        {
          int childpos = 0;
          for( final IChartLayer child : ((IExpandableChartLayer) layer).getLayerManager().getLayers() )
          {
            final IMemento lmem = getMementoChild( memento, MEM_LAYER_VIS, child.getId(), true );
            lmem.putTextData( "" + child.isVisible() );

            final IMemento memChPos = getMementoChild( layerpos, layer.getId(), child.getId(), true );
            memChPos.putTextData( "" + childpos++ );
          }
        }
        final IMemento layeract = getMementoChild( memento, MEM_LAYER_ACT, layer.getId(), true );
        layeract.putTextData( "" + layer.isActive() ); //$NON-NLS-1$
      }
  }

  private IMemento getMementoChild( final IMemento parent, final String siblingId, final String id, final boolean canCreate )
  {
    for( final IMemento children : parent.getChildren( siblingId ) )
      if( children.getID().equals( id ) )
        return children;
    return canCreate ? parent.createChild( siblingId, id ) : null;
  }

  public IProfilLayerProvider getLayerProvider( )
  {
    return m_layerProvider;
  }

  public void setLayerProvider( IProfilLayerProvider layerProvider )
  {
    m_layerProvider = layerProvider;
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( Class< ? > clazz )
  {
    if( clazz == IChartPart.class )
      return this;
    return super.getAdapter( clazz );
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getAxisDragHandler()
   */
  @Override
  public AxisDragHandlerDelegate getAxisDragHandler( )
  {
    return m_axisDragHandler;
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getChartComposite()
   */
  @Override
  public ChartComposite getChartComposite( )
  {
    return m_chart;
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getPlotDragHandler()
   */
  @Override
  public PlotDragHandlerDelegate getPlotDragHandler( )
  {
    return m_plotDragHandler;
  }

}